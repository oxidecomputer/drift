// Copyright 2025 Oxide Computer Company

use std::borrow::Cow;

use anyhow::anyhow;
use openapiv3::ReferenceOr;
use serde::{Deserialize, de::DeserializeOwned};

use crate::context::Context;

pub trait ReferenceOrResolver<'a, 'context, T>
where
    T: Clone,
{
    fn resolve(
        &'a self,
        context: &Context<'context>,
    ) -> anyhow::Result<(Cow<'a, T>, Context<'context>)>;
}

impl<'a, 'context, T> ReferenceOrResolver<'a, 'context, T> for ReferenceOr<T>
where
    T: DeserializeOwned + Clone,
{
    fn resolve(
        &'a self,
        context: &Context<'context>,
    ) -> anyhow::Result<(Cow<'a, T>, Context<'context>)> {
        let mut context = context.clone();
        let mut target = match self {
            ReferenceOr::Item(item) => return Ok((Cow::Borrowed(item), context)),
            ReferenceOr::Reference { reference } => Cow::Borrowed(reference),
        };

        loop {
            assert!(target.starts_with("#/"));
            context = context.push(target.as_ref());

            let subtree = context
                .raw_openapi
                .pointer(&target.as_str()[1..])
                .ok_or(anyhow!("invalid reference {target}"))?;

            let item_or_reference = ReferenceOr::<T>::deserialize(subtree)?;

            match item_or_reference {
                ReferenceOr::Item(item) => return Ok((Cow::Owned(item), context)),

                ReferenceOr::Reference { reference } => {
                    target = Cow::Owned(reference);
                }
            }
        }
    }
}
