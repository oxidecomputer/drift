// Copyright 2025 Oxide Computer Company

use std::fmt;

use crate::JsonPathStack;

// Describes any change detected between two OpenAPI documents.
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

    /// Details on the kind of change.
    pub details: ChangeDetails,
}

// Format `Change` in the nested `paths`/`changes` layout that will be
// introduced when the type is restructured into `Change`, `ChangePath`, and
// `ChangeInfo`. Doing this ahead of time keeps the restructuring commit free
// of test-output noise.
impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        /// Wrapper that formats as `ChangePath { old, new, comparison }`.
        struct PathFmt<'a> {
            old: &'a JsonPathStack,
            new: &'a JsonPathStack,
            comparison: &'a ChangeComparison,
        }

        impl fmt::Debug for PathFmt<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct("ChangePath")
                    .field("old", self.old)
                    .field("new", self.new)
                    .field("comparison", self.comparison)
                    .finish()
            }
        }

        /// Wrapper that formats as `ChangeInfo { old_subpath, new_subpath,
        /// message, class, details }`.
        struct InfoFmt<'a> {
            message: &'a str,
            class: &'a ChangeClass,
            details: &'a ChangeDetails,
        }

        impl fmt::Debug for InfoFmt<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct("ChangeInfo")
                    .field("old_subpath", &"")
                    .field("new_subpath", &"")
                    .field("message", &self.message)
                    .field("class", self.class)
                    .field("details", self.details)
                    .finish()
            }
        }

        let path = PathFmt {
            old: &self.old_path,
            new: &self.new_path,
            comparison: &self.comparison,
        };
        let info = InfoFmt {
            message: &self.message,
            class: &self.class,
            details: &self.details,
        };

        f.debug_struct("Change")
            .field("paths", &[path])
            .field("changes", &[info])
            .finish()
    }
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
