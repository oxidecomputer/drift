// Copyright 2026 Oxide Computer Company

use crate::JsonPathStack;

/// A paired path through old and new documents that led to a change.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangePath {
    /// Path in the old document.
    pub old: JsonPathStack,
    /// Path in the new document.
    pub new: JsonPathStack,
    /// Context in which this path was traversed (Input or Output).
    pub comparison: ChangeComparison,
}

/// Describes changes detected within a single component or endpoint.
#[derive(Debug)]
pub struct Change {
    /// All paths through which this component/endpoint was reached.
    ///
    /// For schema changes, this may contain multiple paths if the same schema
    /// is referenced from multiple locations. For endpoint changes, this will
    /// typically contain a single path.
    pub paths: Vec<ChangePath>,

    /// Individual changes detected within this component/endpoint.
    pub changes: Vec<ChangeInfo>,
}

/// A single change detected at a specific location within a component/endpoint.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangeInfo {
    /// The relative path in the old document where the change occurred.
    ///
    /// For example, `properties/name` for a change at
    /// `#/components/schemas/User/properties/name`.
    ///
    /// This is an empty string if the change is at the component/endpoint root
    /// itself.
    pub old_subpath: String,

    /// The relative path in the new document where the change occurred.
    pub new_subpath: String,

    /// Human-readable message describing the nature of the change.
    pub message: String,

    /// Classification of the change compatibility.
    pub class: ChangeClass,

    /// Details on the kind of change.
    pub details: ChangeDetails,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeComparison {
    // Inputs such as operation parameters and request bodies.
    Input,
    // Outputs such as operation responses.
    Output,
    // Other structures with an OpenAPI document such as operations or
    // metadata.
    Structural,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeClass {
    BackwardIncompatible,
    ForwardIncompatible,
    Incompatible,
    Trivial,
    Unhandled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
