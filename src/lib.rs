// Copyright 2025 Oxide Computer Company

//! Drift
//!
//! Detect changes between OpenAPI documents.

mod change;
mod compare;
mod context;
mod operations;
mod resolve;
mod schema;
mod setops;

use std::fmt::Debug;

pub use change::*;
pub use compare::compare;

/// Represents a location in an OpenAPI document.
///
/// This takes the the form of a stack of JSON paths where each element of the
/// stack starts at the document root and terminates in either a reference
/// (i.e. to the subsequent element in the stack) or the item being identified.
#[derive(Clone)]
pub struct JsonPathStack {
    top: String,
    stack: Vec<String>,
}

impl Debug for JsonPathStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = f.debug_list();
        out.entry(&self.top);
        self.stack.iter().rev().for_each(|path| {
            let _ = out.entry(path);
        });
        out.finish()
    }
}

impl JsonPathStack {
    fn new() -> Self {
        Self {
            top: "#".to_string(),
            stack: Vec::new(),
        }
    }

    fn append(&self, segment: &str) -> JsonPathStack {
        let Self { top, stack } = self;

        Self {
            top: format!("{top}/{segment}"),
            stack: stack.clone(),
        }
    }

    fn push(&self, path: &str) -> JsonPathStack {
        let Self { top, stack } = self;
        let mut stack = stack.clone();
        stack.push(format!("{top}/$ref"));

        Self {
            top: path.to_string(),
            stack,
        }
    }

    pub fn contains_cycle(&self) -> bool {
        self.stack.iter().any(|item| item.starts_with(&self.top))
    }

    pub fn iter(&self) -> impl Iterator<Item = &String> {
        std::iter::once(&self.top).chain(self.stack.iter().rev())
    }
}

impl Default for JsonPathStack {
    fn default() -> Self {
        Self::new()
    }
}
