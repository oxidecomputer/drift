// Copyright 2025 Oxide Computer Company

use std::{borrow::Cow, ops::Deref};

use openapiv3::ReferenceOr;
use serde::de::DeserializeOwned;
use serde_json::Value;

use crate::{JsonPathStack, resolve::ReferenceOrResolver};

#[derive(Clone, Debug)]
pub struct Context<'a> {
    pub raw_openapi: &'a Value,
    pub stack: JsonPathStack,
}

impl<'a> Context<'a> {
    pub fn new(raw_openapi: &'a Value) -> Self {
        Self {
            raw_openapi,
            stack: JsonPathStack::new(),
        }
    }

    pub fn append(&self, segment: &str) -> Context<'a> {
        let stack = self
            .stack
            .append(&segment.replace("~", "~0").replace("/", "~1"));

        Self {
            raw_openapi: self.raw_openapi,
            stack,
        }
    }

    pub(crate) fn push(&self, path: &str) -> Context<'a> {
        let stack = self.stack.push(path);

        Self {
            raw_openapi: self.raw_openapi,
            stack,
        }
    }

    pub fn stack(&self) -> &JsonPathStack {
        &self.stack
    }
}

#[derive(Clone)]
pub struct Contextual<'a, T> {
    context: Context<'a>,
    value: T,
}

impl<'a, T> Contextual<'a, T> {
    pub fn new(context: Context<'a>, value: T) -> Self {
        Self { context, value }
    }

    pub fn append_deref<'s, S>(&'s self, field: &'s S, segment: &str) -> Contextual<'a, &'s S> {
        Contextual {
            context: self.context.append(segment),
            value: field,
        }
    }

    pub fn subcomponent<'s, S>(&'s self, field: &'s S) -> Contextual<'a, &'s S> {
        Contextual {
            context: self.context.clone(),
            value: field,
        }
    }

    pub fn context(&self) -> &Context<'a> {
        &self.context
    }
}

impl<T> Deref for Contextual<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> AsRef<T> for Contextual<'_, T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<'a, T> Contextual<'a, &ReferenceOr<T>>
where
    T: DeserializeOwned + Clone,
{
    pub fn contextual_resolve(&self) -> anyhow::Result<(Cow<'_, T>, Context<'a>)> {
        self.value.resolve(&self.context)
    }
}

pub(crate) trait ToContext<'a> {
    fn to_context(&self) -> &Context<'a>;
}

impl<'context> ToContext<'context> for Context<'context> {
    fn to_context(&self) -> &Context<'context> {
        self
    }
}

impl<'context, T> ToContext<'context> for Contextual<'context, T> {
    fn to_context(&self) -> &Context<'context> {
        self.context()
    }
}
