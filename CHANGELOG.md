# Changelog

All notable changes to this project are documented in this file.

The format is inspired by Keep a Changelog, and this project follows semantic-style versioning for releases.

## [0.1.0] - 2026-02-22

### Added
- Multi-package layout:
  - `pkgs/string-tools-lib` for implementation.
  - `pkgs/string-tools` for documentation.
- Public `string-tools` module export path intended for:
  - `(require string-tools)`
- Character-set implementation and API in:
  - `string-tools/char-set`
- Large set of string utilities, including:
  - slicing/indexing helpers, trimming, searching, partitioning, escaping/quoting,
    line/newline utilities, formatting/layout helpers, similarity metrics, and transforms.
- Extended documentation with:
  - function overview
  - reference entries
  - extended worked examples (logs, CSV-like cleaning, config normalization)
- MIT license metadata for both packages.
- Extensive RackUnit tests, including deterministic property-style tests for:
  - index normalization
  - `string-slice/step`
  - `string-find-all-needle`
  - matcher-equivalence across several APIs

### Changed
- Documentation conventions and layout refined:
  - section ordering
  - argument naming clarity
  - callouts/margin notes
  - staged long-form examples
- Build instructions standardized to render docs into `html/`.

### Fixed
- `string-slice/step` negative-step bounds handling bug found by property tests.
- Scribble cross-reference issues and undefined-tag warnings in docs build.

