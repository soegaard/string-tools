# Contributing

## Local Setup

Install local packages:

```sh
raco pkg install --auto --name string-tools-lib pkgs/string-tools-lib
raco pkg install --auto --name string-tools pkgs/string-tools
```

Quick smoke check:

```sh
racket -e '(require string-tools)'
racket -e '(require string-tools/char-set)'
```

## Validation Before Commit

Run tests:

```sh
raco test pkgs/string-tools-lib/string-tools.rkt
```

Build docs:

```sh
raco scribble --html +m --dest html --redirect-main https://docs.racket-lang.org/ pkgs/string-tools/string-tools.scrbl
```

## Implementation Style

- Use `#lang racket/base` for implementation files unless there is a clear reason not to.
- Prefer standard Racket libraries over SRFI dependencies unless SRFI use is explicitly requested.
- For exported and internal helpers, include:
  - contract comment (`;; name : ... -> ...`)
  - purpose comment (`;;   ...`)
- Prefer `cond` for non-trivial branching.
- Align RHS expressions for consecutive `define`s and `cond` clauses when it improves readability.

## Documentation Style

- Keep prose direct and practical.
- Avoid half-open interval notation in prose; prefer plain wording.
- Keep normal `@examples` labels for regular entries.
- Use `#:label #f` only for staged long worked examples.
- For long examples, split into smaller blocks with one-line lead-ins.
- Do not leave unresolved Scribble warnings after doc edits unless explicitly accepted.

## Changelog

For user-visible changes, update:

- `/Users/soegaard/Dropbox/GitHub/string-tools/CHANGELOG.md`

Use `Added`, `Changed`, and `Fixed` sections as appropriate.
