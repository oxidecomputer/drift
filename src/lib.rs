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
        self.stack
            .iter()
            .any(|item| is_path_ancestor_of(&self.top, item))
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

/// Check if `ancestor` is a path-segment-aligned prefix of `path`.
///
/// Returns `true` if either of the following conditions are true:
///
/// 1. `ancestor` is the same as `path`.
/// 2. `ancestor` is a prefix of `path`, and the character immediately
///    following the prefix is `/`.
///
/// The second condition prevents false matches where schema names share a
/// common string prefix (e.g., `Item` matching `ItemPage`).
fn is_path_ancestor_of(ancestor: &str, path: &str) -> bool {
    path.starts_with(ancestor)
        && path
            .as_bytes()
            .get(ancestor.len())
            .is_none_or(|&b| b == b'/')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contains_cycle() {
        // A genuine cycle: pushing back to a schema we've already visited.
        let stack = JsonPathStack::new()
            .push("#/components/schemas/Tree")
            .append("properties")
            .append("children")
            .push("#/components/schemas/Tree");

        assert!(stack.contains_cycle());

        // Schemas whose names share a common string prefix must not be treated
        // as cycles. e.g. the string "Item" is a prefix of "ItemPage", but they
        // are different schemas.
        let stack = JsonPathStack::new()
            .push("#/components/schemas/ItemPage")
            .append("properties")
            .append("items")
            .append("items")
            .push("#/components/schemas/Item");

        assert!(!stack.contains_cycle());
    }

    #[test]
    fn test_is_path_ancestor_of_basics() {
        // Exact match.
        assert!(is_path_ancestor_of(
            "#/components/schemas/Item",
            "#/components/schemas/Item"
        ));

        // Proper ancestor.
        assert!(is_path_ancestor_of(
            "#/components/schemas/Item",
            "#/components/schemas/Item/properties/value"
        ));

        // Shared string prefix but not a path ancestor.
        assert!(!is_path_ancestor_of(
            "#/components/schemas/Item",
            "#/components/schemas/ItemPage"
        ));

        // Sibling, not ancestor.
        assert!(!is_path_ancestor_of(
            "#/components/schemas/Item",
            "#/components/schemas/Other"
        ));
    }
}
