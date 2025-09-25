// Copyright 2025 Oxide Computer Company

use std::{collections::BTreeMap, sync::LazyLock};

use anyhow::anyhow;
use openapiv3::{OpenAPI, Operation, Parameter, ParameterData, ReferenceOr};
use regex::Regex;
use serde::Deserialize;

use crate::{
    context::{Context, Contextual},
    resolve::ReferenceOrResolver,
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct OperationKey {
    route: String,
    method: String,
}

impl OperationKey {
    fn new(path: &str, method: &str) -> Self {
        // The names of path parameters don't actually matter in terms of
        // distinguishing paths so we eliminate them.
        static PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\{[^}]*\}").unwrap());
        let route = PATTERN.replace_all(path, "{}");

        Self {
            route: route.to_string(),
            method: method.to_string(),
        }
    }
}

pub struct OperationInfo<'a> {
    pub path: String,
    pub operation: Contextual<'a, Operation>,
    pub shared_parameters: Contextual<'a, Vec<ReferenceOr<Parameter>>>,
}

pub fn operations<'a>(
    context: &Context<'a>,
) -> anyhow::Result<Vec<(OperationKey, OperationInfo<'a>)>> {
    let api = OpenAPI::deserialize(context.raw_openapi).unwrap();

    let mut out = Vec::new();

    let context = context.append("paths");

    for (path, ref_or_operation) in api.paths.paths.iter() {
        let context = context.append(path);
        let (path_item, context) = ref_or_operation.resolve(&context)?;

        let shared_parameters =
            Contextual::new(context.append("parameters"), path_item.parameters.clone());

        for (method, operation) in path_item.iter() {
            let context = context.append(method);
            let op_key = OperationKey::new(path, method);
            let op_info = OperationInfo {
                path: path.clone(),
                operation: Contextual::new(context, operation.clone()),
                shared_parameters: shared_parameters.clone(),
            };
            out.push((op_key, op_info))
        }
    }

    Ok(out)
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParameterKey {
    name: ParameterName,
    kind: ParameterKind,
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParameterName {
    Indexed(usize),
    Named(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParameterKind {
    Query,
    Header,
    Path,
    Cookie,
}

pub fn all_params<'a>(
    operation: &'a OperationInfo,
) -> anyhow::Result<Vec<(ParameterKey, Contextual<'a, Parameter>)>> {
    // Generate an indexed map for the names that appear in the path.
    let path_map = path_map(&operation.path);

    let shared = operation
        .shared_parameters
        .iter()
        .enumerate()
        .map(|(idx, ref_or_param)| {
            operation
                .shared_parameters
                .append_deref(ref_or_param, &idx.to_string())
        });
    let param_context = operation.operation.context().append("parameters");
    let local = operation
        .operation
        .parameters
        .iter()
        .enumerate()
        .map(|(idx, ref_or_param)| {
            Contextual::new(param_context.append(&idx.to_string()), ref_or_param)
        });

    shared
        .into_iter()
        .chain(local)
        .map(|param| {
            let (param, param_context) = param.contextual_resolve()?;
            let key = match param.as_ref() {
                Parameter::Query {
                    parameter_data: ParameterData { name, .. },
                    ..
                } => ParameterKey {
                    name: ParameterName::Named(name.clone()),
                    kind: ParameterKind::Query,
                },
                Parameter::Header {
                    parameter_data: ParameterData { name, .. },
                    ..
                } => ParameterKey {
                    name: ParameterName::Named(name.clone()),
                    kind: ParameterKind::Header,
                },
                Parameter::Path {
                    parameter_data: ParameterData { name, .. },
                    ..
                } => {
                    let idx = path_map.get(name).ok_or_else(|| {
                        anyhow!(
                            "named path parameter {name} does not appear in operation path {}",
                            operation.path,
                        )
                    })?;
                    ParameterKey {
                        name: ParameterName::Indexed(*idx),
                        kind: ParameterKind::Path,
                    }
                }

                Parameter::Cookie {
                    parameter_data: ParameterData { name, .. },
                    ..
                } => ParameterKey {
                    name: ParameterName::Named(name.clone()),
                    kind: ParameterKind::Cookie,
                },
            };
            anyhow::Result::Ok((key, Contextual::new(param_context, param.into_owned())))
        })
        .collect()
}

fn path_map(mut input: &str) -> BTreeMap<String, usize> {
    let mut vars = Vec::new();

    while let Some(idx) = input.find('{') {
        input = &input[idx + 1..];
        let end = input.find('}').unwrap();
        let xxx = &input[..end];
        vars.push(xxx.to_string());
        input = &input[end + 1..];
    }

    vars.into_iter()
        .enumerate()
        .map(|(idx, component)| (component, idx))
        .collect()
}
