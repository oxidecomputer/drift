// Copyright 2025 Oxide Computer Company

use crate::JsonPathStack;

// Describes any change detected between two OpenAPI documents.
#[derive(Debug)]
pub struct Change {
    /// Human-readable message describing the nature of the change.
    pub message: String,
    /// The path in the old document where the change was detected.
    pub old_path: JsonPathStack,
    /// The path in the new document where the change was detected.
    pub new_path: JsonPathStack,

    /// The way in which the relevant structures during the comparison.
    pub comparison: ChangeComparison,

    /// Classification of the change compatibility.
    pub class: ChangeClass,

    /// Details on the kind of change
    pub details: ChangeDetails,
}

impl Change {
    pub fn new(
        message: impl ToString,
        old_path: JsonPathStack,
        new_path: JsonPathStack,
        comparison: ChangeComparison,
        class: ChangeClass,
        details: ChangeDetails,
    ) -> Self {
        Self {
            message: message.to_string(),
            old_path,
            new_path,
            comparison,
            class,
            details,
        }
    }
}

#[derive(Debug)]
pub enum ChangeComparison {
    // Inputs such as operation parameters and request bodies.
    Input,
    // Outputs such as operation responses.
    Output,
    // Other structures with an OpenAPI document such as operations or
    // metadata.
    Structural,
}

#[derive(Debug)]
pub enum ChangeClass {
    BackwardIncompatible,
    ForwardIncompatible,
    Incompatible,
    Trivial,
    Unhandled,
}

#[derive(Debug)]
pub enum ChangeDetails {
    Metadata,
    Added,
    Removed,
    AddedRequired,
    MoreStrict,
    LessStrict,
    Datatype,
    UnknownDifference,
}
