// Copyright 2026 Oxide Computer Company

//! Strongly-typed JSON path stack for tracking location in OpenAPI documents.
//!
//! This module provides types for tracking location while traversing an OpenAPI
//! document, particularly when following `$ref` references.
//!
//! ## State machine
//!
//! The [`JsonPathStack`] uses an internal state machine ([`PathState`]) that
//! enforces these invariants at the type level:
//!
//! - **`PathsRoot`**: At `#/paths`, used only for reporting operation add/remove.
//!   Cannot follow refs or append segments from this state.
//! - **`AtEndpoint`**: At an endpoint path (`#/paths/<path>/...`), no refs
//!   followed yet. Can append segments or push a ref.
//! - **`AtComponent`**: At a ref target (any JSON pointer), with a reference
//!   chain. The chain always has exactly one endpoint origin (the first ref),
//!   followed by zero or more intermediates.
//!
//! This makes illegal states unrepresentable: you cannot have endpoint refs
//! mixed into the intermediate chain, or push refs from `PathsRoot`.

use std::fmt;

/// Error returned when `JsonPathStack::push()` receives an invalid reference.
///
/// A valid reference must be a JSON pointer starting with `#/`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct InvalidComponentRef {
    /// The invalid reference that was provided.
    pub(crate) reference: String,
}

impl fmt::Display for InvalidComponentRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid reference {:?}: expected JSON pointer starting with #/",
            self.reference
        )
    }
}

impl std::error::Error for InvalidComponentRef {}

/// An endpoint path, guaranteed to start with `#/paths/`.
///
/// This represents a location within the paths section of an OpenAPI document.
///
/// The `base_len` field stores the length of the path when first constructed
/// (via `for_path` or `for_operation`), before any `append` calls. This allows
/// retrieval of the endpoint base path after segments have been appended.
///
/// Note that `for_path` and `for_operation` produce different base lengths:
/// `for_path("/users")` has base `#/paths/~1users`, while
/// `for_operation("/users", "get")` has base `#/paths/~1users/get`. This
/// distinction matters for change grouping: shared parameters (which live at
/// the path-item level) group under the path-item base, while operation-level
/// constructs group under the operation base.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct EndpointPath {
    path: String,
    /// Length of the path at construction time, before any appends.
    base_len: usize,
}

impl EndpointPath {
    /// Create an endpoint path for a path item (without a specific operation).
    ///
    /// This is used for path-level constructs like shared parameters. (Oxide's
    /// OpenAPI documents do not have path-level constructs.)
    pub(crate) fn for_path(api_path: &str) -> Self {
        let escaped = escape_json_pointer_segment(api_path);
        let path = format!("#/paths/{}", escaped);
        Self {
            base_len: path.len(),
            path,
        }
    }

    /// Create an endpoint path for an operation.
    ///
    /// The `api_path` is the raw API path (like `/users/{id}`) and `method` is
    /// the HTTP method (like `get`). The `api_path` is escaped per JSON pointer
    /// rules; `method` is expected to not need escaping, and is used as-is.
    pub(crate) fn for_operation(api_path: &str, method: &str) -> Self {
        let escaped_path = escape_json_pointer_segment(api_path);
        let path = format!("#/paths/{}/{}", escaped_path, method);
        Self {
            base_len: path.len(),
            path,
        }
    }

    /// Append a path segment, escaping special characters per RFC 6901.
    pub(crate) fn append(&self, segment: &str) -> Self {
        let escaped = escape_json_pointer_segment(segment);
        Self {
            path: format!("{}/{}", self.path, escaped),
            base_len: self.base_len,
        }
    }

    /// Get the JSON pointer string.
    fn as_str(&self) -> &str {
        &self.path
    }

    /// Get the base path (the path at construction time, before any appends).
    fn base_path(&self) -> &str {
        &self.path[..self.base_len]
    }

    /// Get the subpath (the portion appended after construction).
    ///
    /// Returns an empty string if no segments have been appended.
    fn subpath(&self) -> &str {
        let sub = &self.path[self.base_len..];
        // Strip the leading '/' if present.
        sub.strip_prefix('/').unwrap_or(sub)
    }

    /// Return a new EndpointPath with the subpath stripped.
    fn without_subpath(&self) -> Self {
        Self {
            path: self.base_path().to_owned(),
            base_len: self.base_len,
        }
    }
}

/// A JSON pointer reference (starting with `#/`): guaranteed by construction.
///
/// This represents a location after following at least one `$ref`. It can point
/// to any location in the document, not only components.
///
/// The `base_len` field stores the length of the path when first constructed
/// (via `parse`), before any `append` calls. This allows retrieval of the
/// original ref target (e.g., the named type) after path segments have been
/// appended.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct RefTargetPath {
    path: String,
    /// Length of the path at construction time, before any appends.
    base_len: usize,
}

impl RefTargetPath {
    /// Parse from a JSON pointer. Returns `None` if not a valid local ref.
    fn parse(pointer: &str) -> Option<Self> {
        pointer.starts_with("#/").then(|| Self {
            base_len: pointer.len(),
            path: pointer.to_string(),
        })
    }

    /// Append a path segment, escaping special characters per RFC 6901.
    fn append(&self, segment: &str) -> Self {
        let escaped = escape_json_pointer_segment(segment);
        Self {
            path: format!("{}/{}", self.path, escaped),
            base_len: self.base_len,
        }
    }

    /// Get the JSON pointer string.
    fn as_str(&self) -> &str {
        &self.path
    }

    /// Get the base path (the path at construction time, before any appends).
    ///
    /// For a path created from `#/components/schemas/User` and then appended
    /// with `properties`, this returns `#/components/schemas/User`.
    fn base_path(&self) -> &str {
        &self.path[..self.base_len]
    }

    /// Get the subpath (the portion appended after construction).
    ///
    /// Returns an empty string if no segments have been appended.
    fn subpath(&self) -> &str {
        let sub = &self.path[self.base_len..];
        // Strip the leading '/' if present.
        sub.strip_prefix('/').unwrap_or(sub)
    }

    /// Return a new RefTargetPath with the subpath stripped.
    fn without_subpath(&self) -> Self {
        Self {
            path: self.base_path().to_owned(),
            base_len: self.base_len,
        }
    }
}

/// The state of the path stack, encoding the invariants:
///
/// - If refs have been followed, the first was always from an endpoint.
/// - All subsequent refs form a chain of intermediate locations.
/// - Transitions: endpoint → ref target, or ref target → ref target.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum PathState {
    /// At the paths root (`#/paths`), used only for operation add/remove reporting.
    /// Cannot follow refs from this state.
    PathsRoot,

    /// At an endpoint, no refs followed yet.
    AtEndpoint(EndpointPath),

    /// At a component, with reference chain.
    AtComponent {
        /// The current component location.
        current: RefTargetPath,
        /// Where the first ref was followed from (always an endpoint, with
        /// `/$ref` appended).
        origin_ref: EndpointPath,
        /// Intermediate refs (all from components, each with `/$ref` appended).
        intermediate_refs: Vec<RefTargetPath>,
    },
}

/// Strongly-typed path stack for tracking location in OpenAPI documents.
///
/// The stack tracks the location while traversing an OpenAPI document,
/// particularly when following `$ref` references.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct JsonPathStack {
    state: PathState,
}

impl JsonPathStack {
    /// Create a path stack starting at the given endpoint.
    ///
    /// The endpoint must be a valid endpoint path (starting with `#/paths/`).
    pub(crate) fn for_endpoint(endpoint: EndpointPath) -> Self {
        Self {
            state: PathState::AtEndpoint(endpoint),
        }
    }

    /// Create a path stack at `#/paths` (no specific endpoint).
    ///
    /// This is used only when reporting that an operation was added or removed,
    /// where one side doesn't have the operation at all. In normal use, prefer
    /// `for_endpoint()`.
    pub(crate) fn paths_root() -> Self {
        Self {
            state: PathState::PathsRoot,
        }
    }

    /// Return `true` if this stack has a root endpoint (not created via
    /// `paths_root()`).
    #[cfg(test)]
    fn has_root(&self) -> bool {
        !matches!(self.state, PathState::PathsRoot)
    }

    /// Return `true` if the current location is an endpoint (not a schema).
    #[cfg(test)]
    fn is_at_endpoint(&self) -> bool {
        matches!(self.state, PathState::AtEndpoint(_))
    }

    /// Return `true` if the current location is a schema.
    #[cfg(test)]
    fn is_at_component(&self) -> bool {
        matches!(self.state, PathState::AtComponent { .. })
    }

    /// Get the JSON pointer string for the current location.
    pub fn current_pointer(&self) -> &str {
        match &self.state {
            PathState::PathsRoot => "#/paths",
            PathState::AtEndpoint(path) => path.as_str(),
            PathState::AtComponent { current, .. } => current.as_str(),
        }
    }

    /// Get the base component path (the ref target before any appends).
    ///
    /// Returns `Some` when at a component (after following a `$ref`), containing
    /// the original ref target. For example, if we followed a ref to
    /// `#/components/schemas/User` and then appended `properties/name`, this
    /// returns `Some("#/components/schemas/User")`.
    ///
    /// Returns `None` when at an endpoint or paths root (no ref followed).
    pub fn component_base(&self) -> Option<&str> {
        match &self.state {
            PathState::PathsRoot | PathState::AtEndpoint(_) => None,
            PathState::AtComponent { current, .. } => Some(current.base_path()),
        }
    }

    /// Get the base path and subpath for this location.
    ///
    /// The base is the primary entity being tracked:
    ///
    /// - For `PathsRoot`: `#/paths` with empty subpath.
    /// - For endpoints: the endpoint path (e.g., `#/paths/~1users/get`).
    /// - For components: the component path (e.g., `#/components/schemas/User`).
    ///
    /// The subpath is the relative path from the base to the current location
    /// (e.g., `parameters/0` or `properties/name`). Empty if at the base itself.
    pub fn base_and_subpath(&self) -> (&str, &str) {
        match &self.state {
            PathState::PathsRoot => ("#/paths", ""),
            PathState::AtEndpoint(path) => (path.base_path(), path.subpath()),
            PathState::AtComponent { current, .. } => (current.base_path(), current.subpath()),
        }
    }

    /// Return a new JsonPathStack with the subpath stripped from the current
    /// location.
    ///
    /// This only affects the top of the stack (the current endpoint or
    /// component); intermediate refs are preserved unchanged.
    pub fn without_subpath(&self) -> Self {
        let state = match &self.state {
            PathState::PathsRoot => PathState::PathsRoot,
            PathState::AtEndpoint(path) => PathState::AtEndpoint(path.without_subpath()),
            PathState::AtComponent {
                current,
                origin_ref,
                intermediate_refs,
            } => PathState::AtComponent {
                current: current.without_subpath(),
                origin_ref: origin_ref.clone(),
                intermediate_refs: intermediate_refs.clone(),
            },
        };
        Self { state }
    }

    /// Return the endpoint path this stack is at or originated from as a JSON
    /// pointer, including the leading `#/paths`, but not including subpaths
    /// within the endpoint.
    ///
    /// This can be used to enumerate all endpoints that have changes in them.
    ///
    /// Returns `None` if an endpoint was added or removed on the other side.
    pub fn endpoint_base(&self) -> Option<&str> {
        match &self.state {
            PathState::PathsRoot => None,
            PathState::AtEndpoint(path) => Some(path.base_path()),
            PathState::AtComponent { origin_ref, .. } => Some(origin_ref.base_path()),
        }
    }

    /// Append a path segment to the current location.
    ///
    /// This does not push a new reference; it extends the current path.
    ///
    /// # Panics
    ///
    /// Panics if called on a `paths_root()` stack (use `for_endpoint()` for traversal).
    pub(crate) fn append(&self, segment: &str) -> JsonPathStack {
        let state = match &self.state {
            PathState::PathsRoot => {
                panic!("cannot append to paths_root (use for_endpoint for traversal)")
            }
            PathState::AtEndpoint(path) => PathState::AtEndpoint(path.append(segment)),
            PathState::AtComponent {
                current,
                origin_ref,
                intermediate_refs,
            } => PathState::AtComponent {
                current: current.append(segment),
                origin_ref: origin_ref.clone(),
                intermediate_refs: intermediate_refs.clone(),
            },
        };
        Self { state }
    }

    /// Push a schema reference onto the stack.
    ///
    /// This records the current location (with `/$ref` appended) in the
    /// reference chain and sets the new current location to the schema path.
    ///
    /// Returns an error if `reference` is not a valid local schema path
    /// (`#/...`).
    ///
    /// # Panics
    ///
    /// Panics if called on a `paths_root()` stack (a programming error).
    pub(crate) fn push(&self, reference: &str) -> Result<JsonPathStack, InvalidComponentRef> {
        let schema = RefTargetPath::parse(reference).ok_or_else(|| InvalidComponentRef {
            reference: reference.to_string(),
        })?;

        let state = match &self.state {
            PathState::PathsRoot => {
                panic!("cannot push from paths_root (no endpoint context)")
            }
            PathState::AtEndpoint(path) => PathState::AtComponent {
                current: schema,
                origin_ref: path.append("$ref"),
                intermediate_refs: Vec::new(),
            },
            PathState::AtComponent {
                current,
                origin_ref,
                intermediate_refs,
            } => {
                let mut new_intermediates = intermediate_refs.clone();
                new_intermediates.push(current.append("$ref"));
                PathState::AtComponent {
                    current: schema,
                    origin_ref: origin_ref.clone(),
                    intermediate_refs: new_intermediates,
                }
            }
        };
        Ok(Self { state })
    }

    /// Check if the stack contains a cycle.
    ///
    /// A cycle is detected when the current location is a path-segment-aligned
    /// prefix of any entry in the reference chain. This means the current
    /// schema (or an ancestor of it) was already visited, so descending into
    /// it again would loop forever.
    pub fn contains_cycle(&self) -> bool {
        match &self.state {
            PathState::PathsRoot | PathState::AtEndpoint(_) => false,
            PathState::AtComponent {
                current,
                origin_ref,
                intermediate_refs,
            } => {
                // In OAS 3.0, paths can `$ref` other paths, so we can't
                // only look within intermediate_refs to check for cycles:
                // there's a chance of a cycle involving the origin as well.
                let current_str = current.as_str();
                is_path_ancestor_of(current_str, origin_ref.as_str())
                    || intermediate_refs
                        .iter()
                        .any(|r| is_path_ancestor_of(current_str, r.as_str()))
            }
        }
    }

    /// Iterate over the path reference stack from top (current) to bottom
    /// (origin).
    ///
    /// This yields the current path first, then the reference chain in reverse
    /// order (most recent reference first, origin endpoint last).
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        match &self.state {
            PathState::PathsRoot => {
                Box::new(std::iter::once("#/paths")) as Box<dyn Iterator<Item = &str>>
            }
            PathState::AtEndpoint(path) => {
                Box::new(std::iter::once(path.as_str())) as Box<dyn Iterator<Item = &str>>
            }
            PathState::AtComponent {
                current,
                origin_ref,
                intermediate_refs,
            } => Box::new(
                std::iter::once(current.as_str())
                    .chain(intermediate_refs.iter().rev().map(RefTargetPath::as_str))
                    .chain(std::iter::once(origin_ref.as_str())),
            ),
        }
    }
}

impl fmt::Debug for JsonPathStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format as a list of strings for compatibility with existing tests.
        let mut out = f.debug_list();
        for path in self.iter() {
            out.entry(&path);
        }
        out.finish()
    }
}

impl fmt::Display for JsonPathStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for path in self.iter() {
            if !first {
                write!(f, " -> ")?;
            }
            write!(f, "{}", path)?;
            first = false;
        }
        Ok(())
    }
}

/// Escape a segment for use in a JSON pointer per RFC 6901.
fn escape_json_pointer_segment(segment: &str) -> String {
    segment.replace('~', "~0").replace('/', "~1")
}

/// Check if `ancestor` is a path-segment-aligned prefix of `path`.
///
/// Returns `true` if `path` starts with `ancestor` and the character
/// immediately following the prefix (if any) is `/`. This prevents false
/// matches where schema names share a common string prefix (e.g., `User`
/// matching `UserProfile`).
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
    fn endpoint_path_for_path() {
        let path = EndpointPath::for_path("/users");
        assert_eq!(path.as_str(), "#/paths/~1users");
        assert_eq!(path.base_path(), "#/paths/~1users");
        assert_eq!(path.subpath(), "");
    }

    #[test]
    fn endpoint_path_for_path_escapes() {
        // Paths with special characters get escaped.
        let path = EndpointPath::for_path("/users/{id}/posts");
        assert_eq!(path.as_str(), "#/paths/~1users~1{id}~1posts");
    }

    #[test]
    fn endpoint_path_for_operation() {
        let path = EndpointPath::for_operation("/users", "get");
        assert_eq!(path.as_str(), "#/paths/~1users/get");
        assert_eq!(path.base_path(), "#/paths/~1users/get");
        assert_eq!(path.subpath(), "");
    }

    #[test]
    fn endpoint_path_append() {
        let path = EndpointPath::for_operation("/users", "get").append("responses");
        assert_eq!(path.as_str(), "#/paths/~1users/get/responses");
        assert_eq!(path.base_path(), "#/paths/~1users/get");
        assert_eq!(path.subpath(), "responses");
    }

    #[test]
    fn endpoint_path_append_escapes() {
        let path = EndpointPath::for_operation("/users", "get")
            .append("foo/bar")
            .append("a~b");
        assert_eq!(path.as_str(), "#/paths/~1users/get/foo~1bar/a~0b");
    }

    #[test]
    fn component_path_parse_valid() {
        let path = RefTargetPath::parse("#/components/schemas/User");
        assert!(path.is_some());
        assert_eq!(path.unwrap().as_str(), "#/components/schemas/User");
    }

    #[test]
    fn component_path_parse_valid_various() {
        // Any JSON pointer is valid.
        assert!(RefTargetPath::parse("#/components/responses/NotFound").is_some());
        assert!(RefTargetPath::parse("#/components/parameters/PageSize").is_some());
        assert!(RefTargetPath::parse("#/paths/~1users/get").is_some());
        assert!(RefTargetPath::parse("#/definitions/User").is_some());
    }

    #[test]
    fn component_path_parse_invalid() {
        // Must start with #/.
        assert!(RefTargetPath::parse("components/schemas/User").is_none());
        assert!(RefTargetPath::parse("/components/schemas/User").is_none());
        assert!(RefTargetPath::parse("https://example.com/schema.json").is_none());
    }

    #[test]
    fn component_path_append() {
        let path = RefTargetPath::parse("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("name");
        assert_eq!(path.as_str(), "#/components/schemas/User/properties/name");
    }

    #[test]
    fn ref_target_path_base_and_subpath() {
        let path = RefTargetPath::parse("#/components/schemas/User").unwrap();
        assert_eq!(path.base_path(), "#/components/schemas/User");
        assert_eq!(path.subpath(), "");

        let appended = path.append("properties").append("name");
        assert_eq!(appended.base_path(), "#/components/schemas/User");
        assert_eq!(appended.subpath(), "properties/name");
    }

    #[test]
    fn ref_target_path_without_subpath() {
        let path = RefTargetPath::parse("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("name");

        let base_only = path.without_subpath();
        assert_eq!(base_only.as_str(), "#/components/schemas/User");
        assert_eq!(base_only.base_path(), "#/components/schemas/User");
        assert_eq!(base_only.subpath(), "");
    }

    #[test]
    fn endpoint_path_without_subpath() {
        let path = EndpointPath::for_operation("/users", "get")
            .append("responses")
            .append("200");
        assert_eq!(path.subpath(), "responses/200");

        let base_only = path.without_subpath();
        assert_eq!(base_only.as_str(), "#/paths/~1users/get");
        assert_eq!(base_only.base_path(), "#/paths/~1users/get");
        assert_eq!(base_only.subpath(), "");
    }

    #[test]
    fn json_path_stack_for_endpoint() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint);

        assert_eq!(stack.current_pointer(), "#/paths/~1users/get");
        assert!(stack.is_at_endpoint());
        assert!(stack.has_root());
    }

    #[test]
    fn json_path_stack_paths_root() {
        let stack = JsonPathStack::paths_root();

        assert_eq!(stack.current_pointer(), "#/paths");
        assert!(!stack.has_root());
    }

    #[test]
    fn json_path_stack_append() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .append("responses")
            .append("200")
            .append("schema");

        assert_eq!(
            stack.current_pointer(),
            "#/paths/~1users/get/responses/200/schema"
        );
        assert!(stack.is_at_endpoint());
    }

    #[test]
    fn json_path_stack_push() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .append("responses")
            .append("200")
            .append("schema")
            .push("#/components/schemas/User")
            .unwrap();

        assert_eq!(stack.current_pointer(), "#/components/schemas/User");
        assert!(stack.is_at_component());

        let entries: Vec<_> = stack.iter().collect();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0], "#/components/schemas/User");
        assert_eq!(entries[1], "#/paths/~1users/get/responses/200/schema/$ref");
    }

    #[test]
    fn json_path_stack_cycle_detection() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("manager");

        // No cycle yet.
        assert!(!stack.contains_cycle());

        // Push a reference back to User, creating a cycle.
        let stack = stack.push("#/components/schemas/User").unwrap();
        assert!(stack.contains_cycle());
    }

    // Schemas whose names share a common string prefix must not be treated as
    // cycles. e.g. "User" is a prefix of "UserProfile" at the string level, but
    // they are different schemas.
    #[test]
    fn cycle_detection_no_false_positive_on_shared_prefix() {
        let endpoint = EndpointPath::for_operation("/users", "get");

        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("manager")
            .push("#/components/schemas/UserProfile")
            .unwrap();

        assert!(!stack.contains_cycle());
    }

    #[test]
    fn cycle_detection_no_false_positive_on_shared_prefix_origin() {
        // The origin endpoint path could also be a string prefix of
        // the current location without being an ancestor.
        let endpoint = EndpointPath::for_operation("/users", "get");

        // Follow a ref to a schema whose path happens to start with
        // the same characters as the origin but at a different location.
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/paths/~1users/get-details")
            .unwrap();

        // "AB" is not "A": no cycle, even though "A" is a string prefix
        // of "AB".
        assert!(!stack.contains_cycle());
    }

    #[test]
    fn cycle_detection_true_positive_through_subpath() {
        // A cycle exists when the current schema is an ancestor of a
        // previously visited location (we'd descend into the same subtree).
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("address")
            .push("#/components/schemas/Address")
            .unwrap()
            .append("properties")
            .append("owner")
            // Cycle back to User: User -> ... -> Address -> ... -> User.
            .push("#/components/schemas/User")
            .unwrap();

        assert!(stack.contains_cycle());
    }

    #[test]
    fn is_path_ancestor_of_basics() {
        // Exact match (after stripping /$ref the paths are equal).
        assert!(is_path_ancestor_of(
            "#/components/schemas/User",
            "#/components/schemas/User/$ref"
        ));

        // True ancestor: current is a parent of the chain entry.
        assert!(is_path_ancestor_of(
            "#/components/schemas/User",
            "#/components/schemas/User/properties/name/$ref"
        ));

        // False: shared string prefix but different schema name.
        assert!(!is_path_ancestor_of(
            "#/components/schemas/User",
            "#/components/schemas/UserProfile/$ref"
        ));

        // False: completely unrelated paths.
        assert!(!is_path_ancestor_of(
            "#/components/schemas/User",
            "#/components/schemas/Address/$ref"
        ));

        // Edge: ancestor equals path exactly (no trailing content).
        assert!(is_path_ancestor_of(
            "#/components/schemas/User",
            "#/components/schemas/User"
        ));
    }

    #[test]
    fn json_path_stack_iter_order() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/A")
            .unwrap()
            .push("#/components/schemas/B")
            .unwrap();

        let entries: Vec<_> = stack.iter().collect();
        assert_eq!(entries.len(), 3);
        // Current is first.
        assert_eq!(entries[0], "#/components/schemas/B");
        // Then refs in reverse order (most recent first).
        assert_eq!(entries[1], "#/components/schemas/A/$ref");
        // Origin is last.
        assert_eq!(entries[2], "#/paths/~1users/get/$ref");
    }

    #[test]
    fn json_path_stack_display() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/User")
            .unwrap();

        let displayed = format!("{}", stack);
        assert_eq!(
            displayed,
            "#/components/schemas/User -> #/paths/~1users/get/$ref"
        );
    }

    #[test]
    fn json_path_stack_push_invalid_ref_returns_error() {
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint);

        let err = stack
            .push("not/a/json/pointer")
            .expect_err("expected error for invalid reference");

        assert_eq!(err.reference, "not/a/json/pointer");
        assert_eq!(
            err.to_string(),
            "invalid reference \"not/a/json/pointer\": \
             expected JSON pointer starting with #/"
        );
    }

    #[test]
    fn json_path_stack_base_and_subpath() {
        // PathsRoot.
        let stack = JsonPathStack::paths_root();
        assert_eq!(stack.base_and_subpath(), ("#/paths", ""));

        // Endpoint at base.
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint);
        assert_eq!(stack.base_and_subpath(), ("#/paths/~1users/get", ""));

        // Endpoint with appended segments.
        let stack = stack.append("responses").append("200");
        assert_eq!(
            stack.base_and_subpath(),
            ("#/paths/~1users/get", "responses/200")
        );

        // Component at base.
        let stack = stack
            .append("schema")
            .push("#/components/schemas/User")
            .unwrap();
        assert_eq!(stack.base_and_subpath(), ("#/components/schemas/User", ""));

        // Component with appended segments.
        let stack = stack.append("properties").append("name");
        assert_eq!(
            stack.base_and_subpath(),
            ("#/components/schemas/User", "properties/name")
        );
    }

    #[test]
    fn json_path_stack_without_subpath() {
        // PathsRoot: no-op.
        let stack = JsonPathStack::paths_root();
        assert_eq!(stack.without_subpath().current_pointer(), "#/paths");

        // Endpoint with appended segments: strips back to base.
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .append("responses")
            .append("200");
        let stripped = stack.without_subpath();
        assert_eq!(stripped.current_pointer(), "#/paths/~1users/get");
        assert_eq!(stripped.base_and_subpath(), ("#/paths/~1users/get", ""));

        // Component with appended segments: strips back to component base,
        // preserving the reference chain.
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint)
            .push("#/components/schemas/User")
            .unwrap()
            .append("properties")
            .append("name");
        let stripped = stack.without_subpath();
        assert_eq!(stripped.current_pointer(), "#/components/schemas/User");
        assert_eq!(
            stripped.base_and_subpath(),
            ("#/components/schemas/User", "")
        );

        // Reference chain is preserved after stripping.
        let entries: Vec<_> = stripped.iter().collect();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0], "#/components/schemas/User");
        assert_eq!(entries[1], "#/paths/~1users/get/$ref");
    }

    #[test]
    fn json_path_stack_component_base() {
        // PathsRoot: no component base.
        assert_eq!(JsonPathStack::paths_root().component_base(), None);

        // Endpoint: no component base.
        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint);
        assert_eq!(stack.component_base(), None);

        // Component at base: returns the ref target.
        let stack = stack.push("#/components/schemas/User").unwrap();
        assert_eq!(stack.component_base(), Some("#/components/schemas/User"));

        // Component with appended segments: still returns the ref target base.
        let stack = stack.append("properties").append("name");
        assert_eq!(stack.component_base(), Some("#/components/schemas/User"));
    }

    #[test]
    fn json_path_stack_endpoint() {
        let stack = JsonPathStack::paths_root();
        assert_eq!(stack.endpoint_base(), None);

        let endpoint = EndpointPath::for_operation("/users", "get");
        let stack = JsonPathStack::for_endpoint(endpoint);
        assert_eq!(stack.endpoint_base(), Some("#/paths/~1users/get"));

        let stack = stack.append("responses").append("200");
        assert_eq!(stack.endpoint_base(), Some("#/paths/~1users/get"));

        let stack = stack
            .append("schema")
            .push("#/components/schemas/User")
            .unwrap();
        assert_eq!(stack.endpoint_base(), Some("#/paths/~1users/get"));

        let stack = stack
            .append("properties")
            .push("#/components/schemas/Address")
            .unwrap();
        assert_eq!(stack.endpoint_base(), Some("#/paths/~1users/get"));
    }
}
