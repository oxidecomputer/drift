# Changelog

<!-- next-header -->
## Unreleased - ReleaseDate

### Added

- Drift now returns every endpoint (and corresponding path) from which a particular changed type is accessible, not just the first.

### Changed

- For every endpoint or component that had any number of changes in it, a single `Change` instance is now returned. `Change` now consists of a `Vec<ChangePath>` and a `Vec<ChangeInfo>`.

## [0.1.4] - 2026-05-06

- evaluate oneOf <-> enum equivalency
- fix an error in cycle detection that would cause subtrees to be ignored

## [0.1.3] - 2026-01-26

Fixed a few missing cases:

- A new optional query param: forward-incompatible
- A request body is removed: forward-incompatible
- Required changed between old and new bodies: backward or forward-incompatible, depending

## [0.1.2] - 2025-12-09

- Improved support for trivial `allOf`/`anyOf`/`oneOf` constructions

## [0.1.1] - 2025-10-08

- Fixed an instance of infinite recursion with cycle schemas

## [0.1.0] - 2025-09-26

Initial release.

<!-- next-url -->
[0.1.3]: https://github.com/oxidecomputer/drift/releases/tag/drift-0.1.3
[0.1.2]: https://github.com/oxidecomputer/drift/releases/tag/drift-0.1.2
[0.1.1]: https://github.com/oxidecomputer/drift/releases/tag/drift-0.1.1
[0.1.0]: https://github.com/oxidecomputer/drift/releases/tag/drift-0.1.0
