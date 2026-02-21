# AGENTS.md

## Scope
This guide applies to code and documentation in this repository.

## Language And Dependencies
- Use `#lang racket/base` for implementation files unless a file explicitly requires something else.
- Prefer standard Racket libraries (for example `racket/list`, `racket/set`, `racket/string`).
- Do not use SRFI dependencies in implementation files unless explicitly requested.

## Function Comment Style
For exported and internal helper functions, add both:
- A contract comment line:
  - `;; name : contract -> result`
- A purpose comment line that starts with two spaces after `;;`:
  - `;;   Brief purpose sentence.`

Example:
- `;; char-set-member? : char-set? char? -> boolean?`
- `;;   Check whether ch is a member of cs.`

## Control-Flow Style
- Prefer `cond` over nested `if`/`let` patterns when branching logic is non-trivial.
- Prefer internal `define`s inside `cond` branches instead of wrapping branch bodies in `let` just to name intermediates.
- Align right-hand-side expressions in `cond` clauses where it improves readability.
- For consecutive `define` forms, align right-hand-side expressions where it improves readability.

## Documentation Style (Scribble)
- Keep prose direct and avoid unnecessary jargon.
- Avoid mentioning Python in user-facing docs unless explicitly requested.
- Avoid half-open interval notation in prose (for example `[start, end)`); use plain wording such as “from `start` to `end`, with `end` not included.”
- Use module-aware rendering for links:
  - `raco scribble --html +m --dest html --redirect-main https://docs.racket-lang.org/ string-tools.scrbl`
- If build instructions are kept inside `.scrbl`, use non-rendered Scribble comments (`@;{ ... }`).

## Testing And Validation
After edits, run the most relevant checks:
- `raco make <file>.rkt`
- `raco test <file>.rkt`
- For docs: `raco scribble --html +m --dest html --redirect-main https://docs.racket-lang.org/ <file>.scrbl`

When failures exist that predate the current change, report them clearly as pre-existing.
