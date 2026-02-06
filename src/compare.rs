// Copyright 2026 Oxide Computer Company

use std::collections::BTreeMap;

use anyhow::Context as _;
use indexmap::{IndexMap, IndexSet};
use openapiv3::{
    MediaType, Operation, Parameter, ParameterSchemaOrContent, ReferenceOr, RequestBody,
};
use serde_json::Value;

use crate::{
    Change, ChangeClass, ChangeComparison, ChangeDetails, ChangeInfo, ChangePath, JsonPathStack,
    context::{Context, Contextual, ToContext},
    operations::{all_params, operations},
    resolve::ReferenceOrResolver,
    schema::SchemaComparison,
    setops::SetCompare,
};

pub fn compare(old: &Value, new: &Value) -> anyhow::Result<Vec<Change>> {
    let mut comp = Compare::default();
    comp.compare(old, new)?;

    let mut changes = Vec::new();

    for record in comp.records.into_values() {
        if !record.changes.is_empty() {
            assert!(
                !record.paths.is_empty(),
                "ChangeRecord has changes so it should have paths"
            );
            changes.push(Change {
                paths: record.paths.into_iter().collect(),
                changes: record.changes.into_iter().collect(),
            });
        }
    }

    Ok(changes)
}

/// The base path of a location in a document, excluding appended segments.
///
/// For a `JsonPathStack` at `#/components/schemas/User/properties/name`, this
/// captures `#/components/schemas/User`. Wrapped in a newtype to prevent
/// confusion with `CurrentPointer` (which includes appended segments).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct BasePath(String);

impl BasePath {
    pub(crate) fn new(stack: &JsonPathStack) -> Self {
        let (base, _) = stack.base_and_subpath();
        Self(base.to_string())
    }
}

/// Key identifying a change location in both documents.
///
/// This key identifies a pair of corresponding base paths (one from each
/// document), such as endpoints or named schemas in `#/components/schemas/`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) struct ChangeKey {
    old: BasePath,
    new: BasePath,
}

/// The full current location in a document, including appended segments.
///
/// This is the path returned by `JsonPathStack::current_pointer()`, wrapped
/// in a newtype to prevent confusion with `BasePath` (which excludes appended
/// segments).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CurrentPointer(String);

impl CurrentPointer {
    pub(crate) fn new(stack: &JsonPathStack) -> Self {
        Self(stack.current_pointer().to_string())
    }
}

/// Key for memoizing schema comparison results.
///
/// This uses the full schema path (including internal paths like
/// `SubType/properties/value`) for accurate memoization. The comparison
/// direction (Input vs Output) is included because compatibility semantics
/// differ based on direction.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VisitedKey {
    comparison: SchemaComparison,
    old_path: CurrentPointer,
    new_path: CurrentPointer,
}

impl VisitedKey {
    pub(crate) fn new(
        comparison: SchemaComparison,
        old_stack: &JsonPathStack,
        new_stack: &JsonPathStack,
    ) -> Self {
        Self {
            comparison,
            old_path: CurrentPointer::new(old_stack),
            new_path: CurrentPointer::new(new_stack),
        }
    }
}

/// Tracks all paths reaching a change location and detected changes within it.
#[derive(Debug)]
pub(crate) struct ChangeRecord {
    paths: IndexSet<ChangePath>,
    // Use an IndexSet here to dedup change info. For types shared between
    // requests and responses, we run through them twice: once with Input and
    // once with Output. Those could potentially produce the same change, which
    // is technically correct but confusing to the user.
    //
    // In the future, we can also do a post-processing step: if the same change
    // is reported as both ForwardIncompatible and BackwardIncompatible, we mark
    // it as (bidirectionally) Incompatible.
    changes: IndexSet<ChangeInfo>,
}

/// Tracks state across the comparison of two OpenAPI documents.
///
/// This struct uses two maps with deliberately different keying strategies:
///
/// - `visited` is keyed by **full path** (`current_pointer`), e.g.,
///   `(SubType/properties/value, SubType/properties/value)`. This memoizes
///   individual node comparisons: "have we already compared these exact
///   schema nodes?" Full paths are required because comparing a schema at
///   its root is a different operation from comparing one of its children.
///   If `visited` used base paths, recursing into `SubType/properties/value`
///   would find the entry for `SubType` and return early, silently skipping
///   the entire subtree.
///
/// - `records` is keyed by **base path** (`base_and_subpath().0`), e.g.,
///   `{SubType, SubType}`. This groups changes by the component or endpoint
///   that owns them. A type change at `SubType/properties/value` belongs to
///   the `SubType` change record alongside root-level metadata changes.
///   Multiple `visited` entries (root + children) feed into a single
///   `records` entry.
///
/// `visited` also includes the comparison direction (Input vs Output) because
/// that changes compatibility: adding an optional field is backward-compatible
/// for output but forward-incompatible for input. `records` omits direction; a
/// single Change groups all changes to a schema regardless of direction, with
/// the direction preserved in each ChangePath's `comparison` field.
///
/// When a schema moves between `$ref` (out-of-line) and inline across
/// versions, the full paths diverge, producing distinct `visited` entries
/// and potentially distinct `records` entries. See the comment in
/// `compare_schema` and the `ref-vs-inline-type-change` test for details.
#[derive(Default)]
pub(crate) struct Compare {
    /// Memoization of schema comparison results, keyed by full path pair.
    pub(crate) visited: BTreeMap<VisitedKey, bool>,
    /// Change records grouped by base path pair (component or endpoint).
    records: BTreeMap<ChangeKey, ChangeRecord>,
}

impl Compare {
    pub fn compare(&mut self, old: &Value, new: &Value) -> anyhow::Result<()> {
        let old_operations = operations(old).context("error deserializing old OpenAPI document")?;
        let new_operations = operations(new).context("error deserializing new OpenAPI document")?;

        let SetCompare {
            a_unique,
            common,
            b_unique,
        } = SetCompare::new(old_operations, new_operations);

        // Any operation that was removed represents a breaking change.
        for op_info in a_unique.values() {
            let op_name = match &op_info.operation.operation_id {
                Some(name) => name.as_str(),
                None => "<unnamed>",
            };
            let new_paths_root = Context::for_paths_root(new);
            self.record_change(
                format!("The operation {op_name} was removed"),
                &op_info.operation,
                &new_paths_root,
                ChangeComparison::Structural,
                ChangeClass::BackwardIncompatible,
                ChangeDetails::Removed,
            );
        }

        // Any new operation is a forward-incompatible change.
        for op_info in b_unique.values() {
            let op_name = match &op_info.operation.operation_id {
                Some(name) => name.as_str(),
                None => "<unnamed>",
            };
            let old_paths_root = Context::for_paths_root(old);
            self.record_change(
                format!("The operation {op_name} was added"),
                &old_paths_root,
                &op_info.operation,
                ChangeComparison::Structural,
                ChangeClass::ForwardIncompatible,
                ChangeDetails::Added,
            );
        }

        // Anything that exists in both requires deeper inspection.
        for (old_info, new_info) in common.values() {
            self.compare_operations(old_info, new_info)?;
        }

        Ok(())
    }

    fn compare_operations(
        &mut self,
        old_operation: &crate::operations::OperationInfo<'_>,
        new_operation: &crate::operations::OperationInfo<'_>,
    ) -> anyhow::Result<()> {
        self.compare_parameters(old_operation, new_operation)?;
        self.compare_request_body(&old_operation.operation, &new_operation.operation)?;
        self.compare_responses(&old_operation.operation, &new_operation.operation)?;
        Ok(())
    }

    fn compare_parameters(
        &mut self,
        old_operation: &crate::operations::OperationInfo<'_>,
        new_operation: &crate::operations::OperationInfo<'_>,
    ) -> anyhow::Result<()> {
        let old_params = all_params(old_operation)?;
        let new_params = all_params(new_operation)?;

        let SetCompare {
            a_unique,
            common,
            b_unique,
        } = SetCompare::new(old_params, new_params);

        // It should not be possible to have arrived here with path parameters
        // added or removed.
        for old_param in a_unique.values().chain(b_unique.values()) {
            assert!(!matches!(old_param.as_ref(), Parameter::Path { .. }));
        }

        // A parameter that has been removed should be ignored, but it's
        // possible that the semantics of the operation change as a result,
        // therefore we treat the removal of a parameter as backward
        // incompatible.
        for old_param in a_unique.values() {
            let param_name = &old_param.parameter_data_ref().name;
            self.record_change(
                format!("The parameter '{param_name}' was removed"),
                old_param,
                &new_operation.operation,
                ChangeComparison::Input,
                ChangeClass::BackwardIncompatible,
                ChangeDetails::Removed,
            );
        }

        // New required parameters will break old clients.
        // New optional parameters are forward-incompatible because new clients
        // might send the parameter to old servers that don't understand it.
        for new_param in b_unique.values() {
            let param_name = &new_param.parameter_data_ref().name;
            if new_param.parameter_data_ref().required {
                self.record_change(
                    format!("A new, required parameter '{param_name}' was added"),
                    &old_operation.operation,
                    new_param,
                    ChangeComparison::Input,
                    ChangeClass::BackwardIncompatible,
                    ChangeDetails::AddedRequired,
                );
            } else {
                self.record_change(
                    format!("A new, optional parameter '{param_name}' was added"),
                    &old_operation.operation,
                    new_param,
                    ChangeComparison::Input,
                    ChangeClass::ForwardIncompatible,
                    ChangeDetails::Added,
                );
            }
        }

        for (old_param, new_param) in common.values() {
            let old_param_data = old_param.parameter_data_ref();
            let new_param_data = new_param.parameter_data_ref();
            let param_name = &new_param_data.name;

            // A parameter that is "more" required is backward incompatible.
            if !old_param_data.required && new_param_data.required {
                self.record_change(
                    format!("The parameter '{param_name}' was optional and is now required"),
                    old_param,
                    new_param,
                    ChangeComparison::Input,
                    ChangeClass::BackwardIncompatible,
                    ChangeDetails::MoreStrict,
                );
            }

            // A parameter that is "less" required is forward incompatible.
            if old_param_data.required && !new_param_data.required {
                self.record_change(
                    format!("The parameter '{param_name}' was required and is now optional"),
                    old_param,
                    new_param,
                    ChangeComparison::Input,
                    ChangeClass::ForwardIncompatible,
                    ChangeDetails::LessStrict,
                );
            }

            match (&old_param_data.format, &new_param_data.format) {
                (
                    ParameterSchemaOrContent::Schema(old_schema),
                    ParameterSchemaOrContent::Schema(new_schema),
                ) => {
                    let old_schema =
                        Contextual::new(old_param.context().append("schema"), old_schema);
                    let new_schema =
                        Contextual::new(new_param.context().append("schema"), new_schema);
                    self.compare_schema_ref(SchemaComparison::Input, old_schema, new_schema)?
                }

                (old, new) if old == new => {}

                _ => {
                    self.record_change(
                        "Unhandled change to parameter schema or content",
                        old_param,
                        new_param,
                        ChangeComparison::Input,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    );
                }
            }
        }

        Ok(())
    }

    fn compare_request_body(
        &mut self,
        old_operation: &Contextual<'_, Operation>,
        new_operation: &Contextual<'_, Operation>,
    ) -> anyhow::Result<()> {
        let old_request_body =
            old_operation.append_deref(&old_operation.request_body, "request_body");
        let new_request_body =
            new_operation.append_deref(&new_operation.request_body, "request_body");

        match (old_request_body.as_ref(), new_request_body.as_ref()) {
            (None, None) => {}
            // A request body is no longer specified. Old clients may still
            // send a body, which the new server won't expect.
            (Some(old_body_or_ref), None) => {
                let contextual_old_body =
                    Contextual::new(old_request_body.context().clone(), old_body_or_ref);
                let (old_body, _) = contextual_old_body.contextual_resolve()?;

                if old_body.required {
                    // Old clients will send a required body that new servers
                    // don't expect.
                    self.record_change(
                        "a required body parameter was removed",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::ForwardIncompatible,
                        ChangeDetails::Removed,
                    );
                } else {
                    // Old clients may send an optional body that new servers
                    // don't expect.
                    self.record_change(
                        "an optional body parameter was removed",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::ForwardIncompatible,
                        ChangeDetails::Removed,
                    );
                }
            }

            (None, Some(new_body_or_ref)) => {
                let contextual_new_body =
                    Contextual::new(new_request_body.context().clone(), new_body_or_ref);
                let (new_body, _) = contextual_new_body.contextual_resolve()?;

                if new_body.required {
                    // This is very much like changing the type of a parameter:
                    // previously the body was "nothing" and now it must be
                    // "something".
                    self.record_change(
                        "no body parameter was specified and now one is required",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::Incompatible,
                        ChangeDetails::AddedRequired,
                    );
                } else {
                    // A new, optional body is backward-compatible because old
                    // clients will not send the body; it is
                    // forward-incompatible because new clients might try to
                    // send a body to an old server.
                    self.record_change(
                        "no body parameter was specified and now one is accepted",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::ForwardIncompatible,
                        ChangeDetails::Added,
                    );
                }
            }
            (Some(old_body_or_ref), Some(new_body_or_ref)) => {
                let (old_body, old_body_context) =
                    old_body_or_ref.resolve(old_request_body.context())?;
                let (new_body, new_body_context) =
                    new_body_or_ref.resolve(new_request_body.context())?;

                // Check if any of the fields changed.
                let RequestBody {
                    description: old_description,
                    content: old_content,
                    required: old_required,
                    extensions: old_extensions,
                } = &*old_body;
                let RequestBody {
                    description: new_description,
                    content: new_content,
                    required: new_required,
                    extensions: new_extensions,
                } = &*new_body;

                // Description and extension changes are trivial metadata changes.
                if old_description != new_description || old_extensions != new_extensions {
                    self.record_change(
                        "the body metadata (description or extensions) changed",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::Trivial,
                        ChangeDetails::Metadata,
                    );
                }

                if !*old_required && *new_required {
                    self.record_change(
                        "the body parameter was optional and is now required",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::BackwardIncompatible,
                        ChangeDetails::MoreStrict,
                    );
                } else if *old_required && !*new_required {
                    self.record_change(
                        "the body parameter was required and is now optional",
                        old_operation,
                        new_operation,
                        ChangeComparison::Input,
                        ChangeClass::ForwardIncompatible,
                        ChangeDetails::LessStrict,
                    );
                }

                let old_body_content =
                    Contextual::new(old_body_context.append("content"), old_content);
                let new_body_content =
                    Contextual::new(new_body_context.append("content"), new_content);

                self.compare_content(
                    SchemaComparison::Input,
                    &old_body_content,
                    &new_body_content,
                )?;
            }
        }
        Ok(())
    }

    fn compare_responses(
        &mut self,
        old_operation: &Contextual<'_, Operation>,
        new_operation: &Contextual<'_, Operation>,
    ) -> anyhow::Result<()> {
        match (
            &old_operation.responses.default,
            &new_operation.responses.default,
        ) {
            (None, None) => {}
            (None, Some(_)) => {
                // Considering the impact of a default response is complex and
                // requires us to consider other responses... and to perhaps
                // apply heuristic knowledge.
                self.record_change(
                    "operation added a default response",
                    old_operation,
                    new_operation,
                    ChangeComparison::Output,
                    ChangeClass::Unhandled,
                    ChangeDetails::Added,
                );
            }
            (Some(_), None) => {
                // Fewer responses is always backward-compatible, but
                // considering the full impact is, again, complex.
                self.record_change(
                    "operation removed a default response",
                    old_operation,
                    new_operation,
                    ChangeComparison::Output,
                    ChangeClass::Unhandled,
                    ChangeDetails::Removed,
                );
            }

            (Some(old_response_or_ref), Some(new_response_or_ref)) => {
                let old_response_or_ref = Contextual::new(
                    old_operation
                        .context()
                        .append("responses")
                        .append("default"),
                    old_response_or_ref,
                );
                let new_response_or_ref = Contextual::new(
                    new_operation
                        .context()
                        .append("responses")
                        .append("default"),
                    new_response_or_ref,
                );

                self.compare_response(old_response_or_ref, new_response_or_ref)?;
            }
        }

        let SetCompare {
            a_unique,
            common,
            b_unique,
        } = SetCompare::new(
            &old_operation.responses.responses,
            &new_operation.responses.responses,
        );

        // It's forward-incompatible for an operation to have responses it no
        // longer sends
        for old_status in a_unique.keys() {
            self.record_change(
                format!("operation no longer responds with status {old_status}"),
                old_operation,
                new_operation,
                ChangeComparison::Output,
                ChangeClass::ForwardIncompatible,
                ChangeDetails::Removed,
            );
        }

        // Adding a new response would break old clients that don't expect it
        // and is therefore backward-incompatible.
        for new_status in b_unique.keys() {
            self.record_change(
                format!("operation added a new response code {new_status}"),
                old_operation,
                new_operation,
                ChangeComparison::Output,
                ChangeClass::BackwardIncompatible,
                ChangeDetails::Added,
            );
        }

        // For common responses, we need to check that the content is
        // compatible.
        for (status, (old_response, new_response)) in common {
            let old_response = Contextual::new(
                old_operation
                    .context()
                    .append("responses")
                    .append(&status.to_string()),
                old_response,
            );
            let new_response = Contextual::new(
                new_operation
                    .context()
                    .append("responses")
                    .append(&status.to_string()),
                new_response,
            );

            self.compare_response(old_response, new_response)?;
        }

        Ok(())
    }

    fn compare_response(
        &mut self,
        old_response_or_ref: Contextual<'_, &ReferenceOr<openapiv3::Response>>,
        new_response_or_ref: Contextual<'_, &ReferenceOr<openapiv3::Response>>,
    ) -> anyhow::Result<()> {
        let (old_response, old_context) = old_response_or_ref.contextual_resolve()?;
        let (new_response, new_context) = new_response_or_ref.contextual_resolve()?;

        let old_content = Contextual::new(old_context.append("content"), &old_response.content);
        let new_content = Contextual::new(new_context.append("content"), &new_response.content);

        self.compare_content(SchemaComparison::Output, &old_content, &new_content)?;

        Ok(())
    }

    fn compare_content(
        &mut self,
        comparison: SchemaComparison,
        old_content: &Contextual<'_, &IndexMap<String, MediaType>>,
        new_content: &Contextual<'_, &IndexMap<String, MediaType>>,
    ) -> anyhow::Result<()> {
        let SetCompare {
            a_unique,
            common,
            b_unique,
        } = SetCompare::new(old_content.iter(), new_content.iter());

        // TODO
        assert!(a_unique.is_empty());
        assert!(b_unique.is_empty());

        for (mime_type, (old_media, new_media)) in common {
            match (&old_media.schema, &new_media.schema) {
                (None, None) => {}
                (None, Some(_)) => {
                    todo!()
                }
                (Some(_), None) => todo!(),
                (Some(old_schema), Some(new_schema)) => {
                    let old_schema = Contextual::new(
                        old_content.context().append(mime_type).append("schema"),
                        old_schema,
                    );
                    let new_schema = Contextual::new(
                        new_content.context().append(mime_type).append("schema"),
                        new_schema,
                    );

                    self.compare_schema_ref(comparison, old_schema, new_schema)?;
                }
            }
        }

        Ok(())
    }

    /// Get or create a `ChangeRecord` for the given path stacks, inserting
    /// the base path into the record's path set.
    fn ensure_record(
        &mut self,
        old_path: &JsonPathStack,
        new_path: &JsonPathStack,
        comparison: ChangeComparison,
    ) -> &mut ChangeRecord {
        let key = ChangeKey {
            old: BasePath::new(old_path),
            new: BasePath::new(new_path),
        };

        let record = self.records.entry(key).or_insert_with(|| ChangeRecord {
            paths: IndexSet::new(),
            changes: IndexSet::new(),
        });

        // Store base-only paths (subpath stripped). IndexSet handles
        // deduplication.
        record.paths.insert(ChangePath {
            old: old_path.without_subpath(),
            new: new_path.without_subpath(),
            comparison,
        });

        record
    }

    /// Record a path reaching a change location without recording a new change.
    ///
    /// This is used when entering a named type to ensure all paths to the type
    /// are recorded, even if we hit the memoization cache and don't record
    /// the change again.
    pub(crate) fn record_path(
        &mut self,
        old: &dyn ToContext<'_>,
        new: &dyn ToContext<'_>,
        comparison: ChangeComparison,
    ) {
        let old_path = old.to_context().stack();
        let new_path = new.to_context().stack();
        self.ensure_record(old_path, new_path, comparison);
    }

    /// Record a change at the given paths.
    ///
    /// This is the unified change recording method. It extracts the base path
    /// from each path stack and uses it as a key to group related changes.
    /// The path (truncated to base) is added to the record's path set, and the
    /// change info is added to the record's changes list.
    pub(crate) fn record_change(
        &mut self,
        message: impl ToString,
        old: &dyn ToContext<'_>,
        new: &dyn ToContext<'_>,
        comparison: ChangeComparison,
        class: ChangeClass,
        details: ChangeDetails,
    ) {
        let old_path = old.to_context().stack();
        let new_path = new.to_context().stack();
        let (_, old_subpath) = old_path.base_and_subpath();
        let (_, new_subpath) = new_path.base_and_subpath();

        let record = self.ensure_record(old_path, new_path, comparison);

        record.changes.insert(ChangeInfo {
            old_subpath: old_subpath.to_string(),
            new_subpath: new_subpath.to_string(),
            message: message.to_string(),
            class,
            details,
        });
    }
}
