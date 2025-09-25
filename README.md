# `drift`

Library to compare two OpenAPI documents to detect changes between them. Those
changes may be classified as backward-incompatible, forward-incompatible,
incompatible (i.e. both backward- and forward-incompatible), trivial (e.g.
changes to document layout or metadata), or unhandled. Unhandled changes are
those where structures are non-identical and `drift` has either not yet been
extended to detect fine-grained compatibility, **or** where compatibility
checks may not be feasible (e.g. comparing the intersection of two regular
expressions).
