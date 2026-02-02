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
    /// This change is backward incompatible. A client on the old version may
    /// not be able to communicate with a server on the new version.
    ///
    /// This represents an input type becoming more restricted (so an old client
    /// can send request data that a new server will reject), or an output type
    /// becoming less restricted (so a new server can send response data that an
    /// old client will reject).
    BackwardIncompatible,
    /// This change is forward incompatible. A server on the old version may
    /// not be able to communicate with a client on the new version.
    ///
    /// This represents an input type becoming less restricted (so a new client
    /// can send request data that an old server will reject), or an output type
    /// becoming more restricted (so an old server can send response data that a
    /// new client will reject).
    ForwardIncompatible,
    /// This change is bidirectionally incompatible. A server on the old version
    /// may not be able to communicate with a client on the new version, and
    /// vice versa.
    Incompatible,
    /// This change is trivial (e.g., metadata changed). Servers and clients
    /// across versions will not be affected.
    Trivial,
    /// This change is not handled by `drift` yet, and it should be assumed to
    /// be bidirectionally incompatible.
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
