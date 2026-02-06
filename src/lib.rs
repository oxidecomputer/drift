// Copyright 2025 Oxide Computer Company

//! Drift
//!
//! Detect changes between OpenAPI documents.

mod change;
mod compare;
mod context;
mod operations;
mod path;
mod resolve;
mod schema;
mod setops;

pub use change::*;
pub use compare::compare;
pub use path::JsonPathStack;
