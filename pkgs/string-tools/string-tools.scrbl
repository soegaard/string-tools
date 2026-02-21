#lang scribble/manual

@;{
Build docs with:
  raco scribble --html +m --redirect-main https://docs.racket-lang.org/ string-tools.scrbl
}

@(require scribble/example
          (only-in scribble/eval make-base-eval)
          "racket-cheat.rkt"
          (for-label racket/base
                     string-tools/char-set
                     string-tools/string-tools))

@(define st-eval (make-base-eval))
@(st-eval '(require string-tools/string-tools))

@title{String Tools}
@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]

@defmodule[string-tools/string-tools]

@section{String Functions}

This section documents string-processing procedures provided by this package.
They focus on practical operations such as splitting, substring counting, and
character counting with explicit index bounds.

@subsection{Conventions}

These conventions apply throughout the string procedures in this section.

@itemlist[
  @item{For procedures that accept @racket[start] and @racket[end], negative
        indices count from the end of the string, and indices are clamped to
        valid bounds.}
  @item{@racket[start] is included and @racket[end] is not included when
        selecting a substring.}
  @item{A value of @racket[-1] denotes the index of the last character.
        Because @racket[end] is not included, using @racket[end] as
        @racket[-1] stops just before the last character.}
  @item{In @racket[string-slice/step], @racket[#f] for @racket[start] or
        @racket[end] means the bound is omitted and defaults according to the
        step direction.}
  @item{In procedures that accept a character matcher, a matcher may be a
        character, a character set, a string (treated as a character set), or
        a unary predicate on characters.}
]

@subsection{Function Index}

Use this overview as a quick map from task to procedure family.

@(CSection
  #:which 'left
  "Access and Slicing"
  (CGroup
   #f
   (CRow "Access"
         @elem{@racket[string-at]})
   (CRow "Slicing"
         @elem{@racket[string-slice] @racket[string-slice/step]})
   (CRow "Split/Replace"
         @elem{@racket[string-split-at] @racket[string-replace-range]})))

@(CSection
  #:which 'left
  "Character Counting"
  (CGroup
   #f
   (CRow "Counting"
         @elem{@racket[string-count] @racket[string-count-lines]})))

@(CSection
  #:which 'left
  "Needle Counting"
  (CGroup
   #f
   (CRow "Needles"
         @elem{@racket[string-count-needle]})))

@(CSection
  #:which 'left
  "Index Search and Trimming"
  (CGroup
   #f
   (CRow "Index/Skip"
         @elem{@racket[string-index] @racket[string-index-right]
               @racket[string-skip] @racket[string-skip-right]})
   (CRow "Trimming"
         @elem{@racket[string-trim-left] @racket[string-trim-right]})))

@(CSection
  #:which 'left
  "Search and Partitioning"
  (CGroup
   #f
   (CRow "Needle Search"
         @elem{@racket[string-find-needle] @racket[string-find-last-needle]
               @racket[string-find-all-needle]})
   (CRow "Partitioning"
         @elem{@racket[string-partition] @racket[string-partition-right] @racket[string-between]})))

@(CSection
  #:which 'left
  "Prefix and Suffix"
  (CGroup
   #f
   (CRow "Normalize"
         @elem{@racket[string-remove-prefix] @racket[string-remove-suffix]
               @racket[string-ensure-prefix] @racket[string-ensure-suffix]})
   (CRow "Common Parts"
         @elem{@racket[string-common-prefix] @racket[string-common-suffix]})))

@(CSection
  #:which 'left
  "Lines"
  (CGroup
   #f
   (CRow "Line Ops"
         @elem{@racket[string-lines] @racket[string-line-start-indices]
               @racket[string-normalize-newlines] @racket[string-chomp]
               @racket[string-chop-newline] @racket[string-ensure-ends-with-newline]})
   (CRow "Tabs/Width"
         @elem{@racket[string-expand-tabs] @racket[string-display-width]})))

@(CSection
  #:which 'right
  "Construction and Transformation"
  (CGroup
   #f
   (CRow "Case/Map"
         @elem{@racket[string-capitalize] @racket[string-swapcase]
               @racket[string-map] @racket[string-map!]})
   (CRow "Transform"
         @elem{@racket[string-repeat] @racket[string-reverse] @racket[string-rot13]
               @racket[string-pluralize] @racket[string-singularize]
               @racket[string-intersperse]})))

@(CSection
  #:which 'right
  "Escaping and Cleaning"
  (CGroup
   #f
   (CRow "Quoting"
         @elem{@racket[string-quote] @racket[string-unquote]})
   (CRow "Visible Escapes"
         @elem{@racket[string-escape-visible] @racket[string-unescape-visible]})
   (CRow "JSON/Regexp/ANSI"
         @elem{@racket[string-escape-json] @racket[string-unescape-json]
               @racket[string-escape-regexp] @racket[string-strip-ansi]
               @racket[string-squeeze]})))

@(CSection
  #:which 'right
  "Tokenization and Scanning"
  (CGroup
   #f
   (CRow "Tokenize/Fields"
         @elem{@racket[string-tokenize] @racket[string-fields]})
   (CRow "Scan"
         @elem{@racket[string-scan]})))

@(CSection
  #:which 'right
  "Formatting and Layout"
  (CGroup
   #f
   (CRow "Layout"
         @elem{@racket[string-wrap] @racket[string-indent]
               @racket[string-dedent] @racket[string-elide]})))

@(CSection
  #:which 'right
  "Similarity and Distance"
  (CGroup
   #f
   (CRow "Metrics"
         @elem{@racket[string-levenshtein] @racket[string-jaro-winkler] @racket[string-similarity]})))

@(CSection
  #:which 'right
  "Case Conversion and Predicates"
  (CGroup
   #f
   (CRow "Predicates"
         @elem{@racket[string-blank?] @racket[string-ascii?] @racket[string-digit?]})))

@(render-cheat-sheet)

@subsection{Splitting and Slicing}

This subsection covers positional extraction and replacement operations, from
safe single-character access to stepped slicing and split-at-index workflows.

@margin-note{ℹ️ Think of @racket[string-slice] as a nicer @racket[substring].}
@defproc[(string-slice
           [s string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         string?]{

Compared to @racket[substring], this procedure accepts negative indices and
clamps out-of-range indices instead of raising bounds errors.

Indices are normalized and clamped to the string bounds:
negative indices count backward from the end, and out-of-range indices are
clamped to valid positions. If the normalized end is less than or equal to the
normalized start, the result is the empty string.

@examples[
  #:eval st-eval
  (string-slice "abcdef")
  (string-slice "abcdef" 1 4)
  (string-slice "abcdef" -3 -1)
  (string-slice "abcdef" -100 100)
  (string-slice "abcdef" 4 2)
]

Related: @racket[string-slice/step], @racket[string-at].
}

@defproc[(string-slice/step
           [s string?]
           [start (or/c exact-integer? #f) #f]
           [end (or/c exact-integer? #f) #f]
           [step exact-integer? 1])
         string?]{

Like @racket[string-slice], but also supports stepping.

@margin-note{@bold{⚠️ Gotcha:} With negative @racket[step], omitted bounds
(@racket[#f]) behave differently from explicit negative indices such as
@racket[-1].}

When @racket[step] is positive, traversal is left to right. When
@racket[step] is negative, traversal is right to left. A zero step raises an
exception.

If @racket[start] or @racket[end] is @racket[#f], the bound is treated as
omitted and defaults according to the step direction.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-slice/step "abcdef")
  (string-slice/step "abcdef" 0 6 2)
  (string-slice/step "abcdef" 5 #f -2)
  (string-slice/step "abcdef" #f #f -1)
]

Related: @racket[string-slice], @racket[string-at].
}

@margin-note{ℹ️ Think of @racket[string-at] as a safer @racket[string-ref].}
@defproc[(string-at
           [s string?]
           [i exact-integer?]
           [default any/c #f])
         any/c]{

Returns the character at index @racket[i].

Indices are clamped to the string bounds, and negative indices count from the
end of the string.

@margin-note{@bold{⚠️ Gotcha:} For non-empty strings, out-of-range indices are
clamped, so @racket[default] is only used when @racket[s] is empty.}

If @racket[s] is empty, @racket[default] is returned.

@examples[
  #:eval st-eval
  (string-at "abc" 0)
  (string-at "abc" -1)
  (string-at "abc" 3)
  (string-at "abc" -10)
  (string-at "" 0 #\x)
]

Related: @racket[string-slice], @racket[string-slice/step].
}

@defproc[(string-split-at
           [s string?]
           [i exact-integer?] ...)
         (listof string?)]{

Splits @racket[s] at the given indices and returns the resulting substrings as
a list.

Indices may be negative and are clamped to the string bounds. The indices may be given in any
order and may contain duplicates; they are sorted and deduplicated before
splitting.

The returned list contains the substrings of @racket[s] between successive cut
positions, including the beginning and end of the string.

@examples[
  #:eval st-eval
  (string-split-at "abcdef" 2 4)
  (string-split-at "abcdef" 4 2)
  (string-split-at "abc" 1 1 2)
  (string-split-at "abc")
  (string-split-at "abc" 0)
  (string-split-at "abc" -1)
  (string-split-at "abc" 3)
]

If no indices are provided, the result is a list containing @racket[s] itself.

If exactly one index is provided, the result is a two-element list consisting
of the prefix and suffix at that index.

An exception is raised if any index is not an exact integer.
}

@defproc[(string-replace-range
           [s string?]
           [start exact-integer?]
           [end exact-integer?]
           [replacement string?])
         string?]{

Replaces the selected portion of @racket[s] with @racket[replacement].

The replaced portion starts at @racket[start] and continues up to
@racket[end], excluding the character at @racket[end].

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-replace-range "abcdef" 2 4 "XY")
  (string-replace-range "abcdefgh" 2 6 "X")
  (string-replace-range "abcdefgh" 2 4 "WXYZ")
  (string-replace-range "abcdef" -4 -2 "XY")
  (string-replace-range "abcdef" 4 2 "XY")
]
}

@subsection{Character Counting}

This subsection covers character-wise counting with flexible matching
criteria, including character, character-set, string, and predicate forms.

@defproc[(string-count
           [s string?]
           [to-count (or/c char? char-set? string? (-> char? any/c))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         exact-nonnegative-integer?]{

Counts characters in @racket[s] that satisfy @racket[to-count]
between @racket[start] and @racket[end].

Indices may be negative and are clamped to the string bounds.

If @racket[to-count] is a procedure, it is applied to each character
as a predicate. If it is a character set, each character is tested for
membership. If it is a character, @racket[char=?] is used. If it is a string,
the string is converted to a character set.

An exception is raised if @racket[start] or @racket[end] is not an exact
integer, or if
@racket[to-count] is not a character, character set, string,
or unary procedure.

@examples[
  #:eval st-eval
  (string-count "banana" #\a)
  (string-count "banana" "an")
  (require string-tools/char-set)
  (string-count "banana" (make-char-set #\a #\n))
  (string-count "a1b2c3" char-numeric?)
  (string-count "banana" #\a 2 6)
  (string-count "banana" #\a -5 -1)
]

This procedure provides character counting with flexible matching criteria.
}

@subsection{Needle Counting}

This subsection groups substring-occurrence counting utilities for bounded
regions of a string. Use these procedures when you need non-overlapping needle
counts rather than per-character counting.

@defproc[(string-count-needle
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         exact-nonnegative-integer?]{

Counts the number of non-overlapping occurrences of @racket[needle] in
@racket[s], restricted to the substring from @racket[start] to
@racket[end] (exclusively).

The search begins at @racket[start] and stops before @racket[end], which
defaults to the length of @racket[s].  In other words, @racket[start] is
included and @racket[end] is not included.

Indices may be negative and are clamped to the string bounds.

Occurrences are counted from left to right and do not overlap.

If @racket[needle] is the empty string, the result is the number of insertion
positions in the selected substring.

An exception is raised if @racket[start] or @racket[end] is not an exact
integer.

@examples[
  #:eval st-eval
  (string-count-needle "banana" "na")
  (string-count-needle "aaaa" "aa")
  (string-count-needle "aaaa" "aaa")
  (string-count-needle "banana" "na" 3 6)
  (string-count-needle "banana" "na" -4 -1)
  (string-count-needle "abc" "")
  (string-count-needle "abc" "" 1 3)
]

This procedure is intended to complement the string-search utilities in
@racketmodname[racket/string] by providing a direct substring-count operation.
}

@subsection{Index Search and Trimming}

This subsection combines left-to-right and right-to-left index/skip operations
with matcher-driven trimming over bounded substring regions.

@defproc[(string-index
           [s string?]
           [to-find (or/c char? char-set? string? (-> char? any/c))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Searches @racket[s] from left to right and returns the first index from
@racket[start] to @racket[end] (@racket[end] not included) whose character
matches @racket[to-find]. Returns @racket[#f] when no match
is found.

Indices may be negative and are clamped to the string bounds.

Matching rules:
@itemlist[
  @item{If @racket[to-find] is a character, @racket[char=?] is used.}
  @item{If it is a character set, membership is tested.}
  @item{If it is a string, the string is converted to a character set.}
  @item{If it is a procedure, the procedure is used as a predicate.}
]

@examples[
  #:eval st-eval
  (string-index "banana" #\a)
  (string-index "banana" "nz")
  (string-index "banana" (make-char-set #\n #\z))
  (string-index "a1b2c3" char-numeric?)
  (string-index "banana" #\a -5 -1)
]

This procedure searches from left to right with configurable matching criteria.
}

@defproc[(string-index-right
           [s string?]
           [to-find (or/c char? char-set? string? (-> char? any/c))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Searches @racket[s] from right to left and returns the first matching index
encountered from @racket[start] to @racket[end] (@racket[end] not included).
Returns @racket[#f] when no match is found.

Indices may be negative and are clamped to the string bounds.

The right-to-left search starts at @racket[(sub1 end)].

@examples[
  #:eval st-eval
  (string-index-right "banana" #\a)
  (string-index-right "banana" "nz")
  (string-index-right "banana" (make-char-set #\n #\z))
  (string-index-right "a1b2c3" char-numeric?)
  (string-index-right "banana" #\a -5 -1)
]

This procedure searches from right to left with configurable matching criteria.
}

@defproc[(string-skip
           [s string?]
           [to-skip (or/c char? char-set? string? (-> char? any/c))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Like @racket[string-index], but uses the complement of the matching criteria:
it searches left to right for the first character from @racket[start] to
@racket[end] (@racket[end] not included),
that does @emph{not} match @racket[to-skip].

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-skip "   abc" #\space)
  (string-skip "aaab" "a")
  (string-skip "123x5" char-numeric?)
  (string-skip "   abc" #\space -4 100)
]

This procedure provides left-to-right skipping using the complement criterion.
}

@defproc[(string-skip-right
           [s string?]
           [to-skip (or/c char? char-set? string? (-> char? any/c))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Like @racket[string-index-right], but uses the complement of the matching
criteria: it searches right to left for the first character in
the substring from @racket[start] to @racket[end]
(@racket[end] not included), that does @emph{not} match @racket[to-skip].

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-skip-right "abc   " #\space)
  (string-skip-right "baaa" "a")
  (string-skip-right "5x321" char-numeric?)
  (string-skip-right "abc   " #\space -100 -1)
]

This procedure provides right-to-left skipping using the complement criterion.
}

@defproc[(string-trim-left
           [s string?]
           [to-trim (or/c char? char-set? string? (-> char? any/c)) char-whitespace?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         string?]{

Returns a copy of the substring from @racket[start] to @racket[end]
(@racket[end] not included), after removing matching characters from the left
side.

Indices may be negative and are clamped to the string bounds.

If @racket[to-trim] is omitted, whitespace is trimmed.
If @racket[to-trim] is a string, it is converted to a character set.

@examples[
  #:eval st-eval
  (string-trim-left "   abc  ")
  (string-trim-left "aaab" #\a)
  (string-trim-left "aaab" "a")
  (string-trim-left "abbaXYZ" "ab")
  (string-trim-left "123x5" char-numeric?)
  (string-trim-left "xxabcxx" #\x 2 7)
  (string-trim-left "xxabcxx" #\x -5 -1)
  (string-trim-left "abcde" #\x 1 3)
]
}

@defproc[(string-trim-right
           [s string?]
           [to-trim (or/c char? char-set? string? (-> char? any/c)) char-whitespace?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         string?]{

Returns a copy of the substring from @racket[start] to @racket[end]
(@racket[end] not included), after removing matching characters from the right
side.

Indices may be negative and are clamped to the string bounds.

If @racket[to-trim] is omitted, whitespace is trimmed.
If @racket[to-trim] is a string, it is converted to a character set.

@examples[
  #:eval st-eval
  (string-trim-right "   abc  ")
  (string-trim-right "baaa" #\a)
  (string-trim-right "baaa" "a")
  (string-trim-right "XYZabba" "ab")
  (string-trim-right "5x321" char-numeric?)
  (string-trim-right "xxabcxx" #\x 1 6)
  (string-trim-right "xxabcxx" #\x -5 -1)
]
}

@subsection{Substring Search and Partitioning}

This subsection groups substring search and partitioning helpers that return
indices, ranges, or before/needle/after splits.

@defproc[(string-find-needle
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Searches @racket[s] from left to right for @racket[needle] from
@racket[start] to @racket[end] (@racket[end] not included). Returns the index
of the first match, or @racket[#f] if no match is found.

Indices may be negative and are clamped to the string bounds.

If @racket[needle] is the empty string, the result is @racket[start].

@examples[
  #:eval st-eval
  (string-find-needle "banana" "na")
  (string-find-needle "banana" "na" 3 6)
  (string-find-needle "banana" "na" -4 -1)
  (string-find-needle "abc" "")
]

This procedure provides direct substring search.

Related: @racket[string-find-all-needle], @racket[string-scan].
}

@defproc[(string-find-last-needle
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (or/c exact-nonnegative-integer? #f)]{

Searches @racket[s] from right to left for @racket[needle] from
@racket[start] to @racket[end] (@racket[end] not included). Returns the index
of the last match, or @racket[#f] if no match is found.

Indices may be negative and are clamped to the string bounds.

If @racket[needle] is the empty string, the result is @racket[end].

@examples[
  #:eval st-eval
  (string-find-last-needle "banana" "na")
  (string-find-last-needle "banana" "na" 0 5)
  (string-find-last-needle "banana" "na" -4 -1)
  (string-find-last-needle "abc" "")
]

This procedure is a right-to-left substring search companion to
@racket[string-find-needle].
}

@defproc[(string-find-all-needle
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)]
           [#:overlap? overlap? boolean? #f]
           [#:ranges? ranges? boolean? #f])
         (listof (or/c exact-nonnegative-integer?
                       (cons/c exact-nonnegative-integer?
                               exact-nonnegative-integer?)))]{

Returns all matches of @racket[needle] from @racket[start] to @racket[end]
(@racket[end] not included).

By default, returns start indices. When @racket[#:ranges?] is true, returns
@racket[(cons start end)] pairs for each match.

When @racket[#:overlap?] is true, overlapping matches are included.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-find-all-needle "banana" "na")
  (string-find-all-needle "banana" "na" #:ranges? #t)
  (string-find-all-needle "aaaa" "aa" #:overlap? #t)
]

Related: @racket[string-find-needle], @racket[string-scan].
}

@defproc[(string-partition
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (values string? string? string?)]{

Searches for the first occurrence of @racket[needle] from @racket[start] to
@racket[end] (@racket[end] not included), and returns three values:

Indices may be negative and are clamped to the string bounds.

@itemlist[
  @item{the substring before the match}
  @item{the matched separator}
  @item{the substring after the match}
]

If no match is found, the second and third values are empty strings, and the
first value is the selected substring.

@examples[
  #:eval st-eval
  (call-with-values (λ () (string-partition "a:b:c" ":")) list)
  (call-with-values (λ () (string-partition "abc" ":")) list)
  (call-with-values (λ () (string-partition "banana" "na")) list)
  (call-with-values (λ () (string-partition "banana" "na" -4 -1)) list)
]
}

@defproc[(string-partition-right
           [s string?]
           [needle string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         (values string? string? string?)]{

Searches for the last occurrence of @racket[needle] from @racket[start] to
@racket[end] (@racket[end] not included), and returns three values:

Indices may be negative and are clamped to the string bounds.

@itemlist[
  @item{the substring before the match}
  @item{the matched separator}
  @item{the substring after the match}
]

If no match is found, the second and third values are empty strings, and the
first value is the selected substring.

@examples[
  #:eval st-eval
  (call-with-values (λ () (string-partition-right "a:b:c" ":")) list)
  (call-with-values (λ () (string-partition-right "abc" ":")) list)
  (call-with-values (λ () (string-partition-right "banana" "na")) list)
  (call-with-values (λ () (string-partition-right "banana" "na" -4 -1)) list)
]
}

@defproc[(string-between
           [s string?]
           [left (or/c char? string?)]
           [right (or/c char? string?)]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)]
           [#:left-match left-match (or/c 'first 'last) 'first]
           [#:right-match right-match (or/c 'first 'last) 'first]
           [#:include-left? include-left? boolean? #f]
           [#:include-right? include-right? boolean? #f])
         (or/c string? #f)]{

Returns the substring between @racket[left] and @racket[right], or @racket[#f]
if delimiters are not found with the selected matching options.

Delimiters may be strings or single characters.

@racket[left-match] and @racket[right-match] choose whether each delimiter uses
its first or last match in the selected bounds. The @racket[include-left?] and
@racket[include-right?] options control whether delimiters are included.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-between "a[b]c" "[" "]")
  (string-between "a[b]c[d]e" "[" "]" #:left-match 'last)
  (string-between "a[b]c[d]e" "[" "]" #:right-match 'last)
  (string-between "a[b]c" "[" "]" #:include-left? #t #:include-right? #t)
  (string-between "a[b]c" #\[ #\])
]
}

@subsection{Prefix and Suffix Utilities}

This subsection provides small prefix/suffix primitives for normalization and
path/key shaping, including remove-if-present and ensure-if-missing forms.

@defproc[(string-remove-prefix
           [s string?]
           [prefix string?])
         string?]{

If @racket[s] starts with @racket[prefix], returns @racket[s] without that
prefix. Otherwise returns @racket[s] unchanged.

@examples[
  #:eval st-eval
  (string-remove-prefix "foobar" "foo")
  (string-remove-prefix "foobar" "bar")
]
}

@defproc[(string-remove-suffix
           [s string?]
           [suffix string?])
         string?]{

If @racket[s] ends with @racket[suffix], returns @racket[s] without that
suffix. Otherwise returns @racket[s] unchanged.

@examples[
  #:eval st-eval
  (string-remove-suffix "foobar" "bar")
  (string-remove-suffix "foobar" "foo")
]
}

@defproc[(string-ensure-prefix
           [s string?]
           [prefix string?])
         string?]{

If @racket[s] does not start with @racket[prefix], returns a new string with
@racket[prefix] prepended. Otherwise returns @racket[s] unchanged.

@examples[
  #:eval st-eval
  (string-ensure-prefix "bar" "foo")
  (string-ensure-prefix "foobar" "foo")
]
}

@defproc[(string-ensure-suffix
           [s string?]
           [suffix string?])
         string?]{

If @racket[s] does not end with @racket[suffix], returns a new string with
@racket[suffix] appended. Otherwise returns @racket[s] unchanged.

@examples[
  #:eval st-eval
  (string-ensure-suffix "foo" "bar")
  (string-ensure-suffix "foobar" "bar")
]
}

@defproc[(string-common-prefix
           [a string?]
           [b string?])
         string?]{

Returns the longest common prefix of @racket[a] and @racket[b].

@examples[
  #:eval st-eval
  (string-common-prefix "foobar" "foobaz")
  (string-common-prefix "abc" "xyz")
]
}

@defproc[(string-common-suffix
           [a string?]
           [b string?])
         string?]{

Returns the longest common suffix of @racket[a] and @racket[b].

@examples[
  #:eval st-eval
  (string-common-suffix "foobar" "xxbar")
  (string-common-suffix "abc" "xyz")
]
}

@subsection{Lines}

This subsection groups line-oriented utilities for text and file processing,
including line splitting, counting, newline normalization, and display-column
handling.

@defproc[(string-lines
           [s string?])
         (listof string?)]{

Splits @racket[s] into lines.

Line separators recognized are @racket[#\newline], @racket[#\return], and the
two-character sequence @racket[#\return] followed by @racket[#\newline].

If @racket[s] ends with a line separator, no extra trailing empty line is
added.

@examples[
  #:eval st-eval
  (string-lines "")
  (string-lines "a\nb")
  (string-lines "a\r\nb")
  (string-lines "a\rb")
  (string-lines "a\n")
]
}

@defproc[(string-count-lines
           [s string?])
         exact-positive-integer?]{

Counts the number of lines in @racket[s].

Line boundaries follow @racket[#\newline], @racket[#\return], and
@racket[#\return] followed by @racket[#\newline]. A @racket[#\return] followed
by @racket[#\newline] counts as one line boundary.

@examples[
  #:eval st-eval
  (string-count-lines "")
  (string-count-lines "a\nb")
  (string-count-lines "a\r\nb")
  (string-count-lines "a\n")
]
}

@defproc[(string-line-start-indices
           [s string?])
         (listof exact-nonnegative-integer?)]{

Returns a list of character offsets for the start of each line in
@racket[s].

Line boundaries follow the same rules as @racket[string-count-lines].

@examples[
  #:eval st-eval
  (string-line-start-indices "")
  (string-line-start-indices "a\nb")
  (string-line-start-indices "a\r\nb")
  (string-line-start-indices "a\n")
]
}

@defproc[(string-normalize-newlines
           [s string?])
         string?]{

Converts line endings in @racket[s] so that every newline sequence becomes a
single @racket[#\newline].

Both @racket[#\return] and @racket[#\return] followed by @racket[#\newline]
are normalized to @racket[#\newline].

@examples[
  #:eval st-eval
  (string-normalize-newlines "a\r\nb")
  (string-normalize-newlines "a\rb")
  (string-normalize-newlines "\r\n\rx\r")
]
}

@defproc[(string-expand-tabs
           [s string?]
           [#:tab-width tab-width exact-positive-integer? 8]
           [#:start-column start-column exact-nonnegative-integer? 0])
         string?]{

Replaces tab characters in @racket[s] with spaces according to tab stops.

Tabs advance to the next tab stop determined by @racket[tab-width]. Newline and
return characters reset the running column to zero.

@examples[
  #:eval st-eval
  (string-expand-tabs "a\tb")
  (string-expand-tabs "ab\tcd" #:tab-width 4)
  (string-expand-tabs "\t" #:tab-width 4 #:start-column 2)
]
}

@defproc[(string-display-width
           [s string?]
           [#:tab-width tab-width exact-positive-integer? 8]
           [#:start-column start-column exact-nonnegative-integer? 0])
         exact-nonnegative-integer?]{

Returns a monospace display-width approximation for the length of the final line of
@racket[s].

ASCII printable characters count as width 1. Tabs advance to the next tab stop.
Newline and return reset the running column to zero.

@examples[
  #:eval st-eval
  (string-display-width "a\tb")
  (string-display-width "a\nbc")
  (string-display-width "\t" #:tab-width 4 #:start-column 2)
]
}

@defproc[(string-chomp
           [s string?])
         string?]{

Removes one trailing newline from @racket[s] when present.
The removed newline may be either @racket["\n"] or @racket["\r\n"].

If no trailing newline is present, @racket[s] is returned unchanged.

@examples[
  #:eval st-eval
  (string-chomp "abc")
  (string-chomp "abc\n")
  (string-chomp "abc\r\n")
  (string-chomp "abc\n\n")
]
}

@defproc[(string-chop-newline
           [s string?])
         string?]{

Alias of @racket[string-chomp].
}

@subsection{String Construction and Transformation}

This subsection collects string-building and transformation utilities, from
repetition and case conversion to simple linguistic and mapping helpers.

@defproc[(string-repeat
           [s string?]
           [n exact-nonnegative-integer?])
         string?]{

Returns a string consisting of @racket[s] repeated @racket[n] times.

@examples[
  #:eval st-eval
  (string-repeat "ab" 0)
  (string-repeat "ab" 3)
]
}

@defproc[(string-reverse
           [s string?])
         string?]{

Returns @racket[s] with its characters in reverse order.

@examples[
  #:eval st-eval
  (string-reverse "")
  (string-reverse "abc")
]
}

@defproc[(string-capitalize
           [s string?])
         string?]{

Returns a string where the first character is uppercased and the remaining
characters are lowercased.

Use @racket[string-upcase] when every character should be uppercased. Use
@racket[string-titlecase] for title-casing behavior across words.

@examples[
  #:eval st-eval
  (string-capitalize "")
  (string-capitalize "hello world")
  (string-capitalize "hELLO WORLD")
]
}

@defproc[(string-swapcase
           [s string?])
         string?]{

Returns a string where uppercase letters are converted to lowercase and
lowercase letters are converted to uppercase. Non-letter characters are
unchanged.

@examples[
  #:eval st-eval
  (string-swapcase "")
  (string-swapcase "AbC")
  (string-swapcase "hello WORLD")
]
}

@margin-note{🌐 See
@hyperlink["https://en.wikipedia.org/wiki/ROT13"]{ROT13 on Wikipedia}.}
@defproc[(string-rot13
           [s string?])
         string?]{

Applies ROT13 to ASCII letters in @racket[s].

@examples[
  #:eval st-eval
  (string-rot13 "Hello, World!")
  (string-rot13 (string-rot13 "Racket"))
  (string-rot13 (string-rot13 "uryyb"))
]
}

@defproc[(string-pluralize
           [s string?])
         string?]{

Returns a simple English-ish plural form of @racket[s] using lightweight
heuristics.

@examples[
  #:eval st-eval
  (string-pluralize "cat")
  (string-pluralize "box")
  (string-pluralize "city")
]
}

@defproc[(string-singularize
           [s string?])
         string?]{

Returns a simple English-ish singular form of @racket[s] using lightweight
heuristics.

@margin-note{@bold{⚠️ Gotcha:} This is heuristic, not full linguistic
inflection.}

@examples[
  #:eval st-eval
  (string-singularize "cats")
  (string-singularize "boxes")
  (string-singularize "cities")
]
}

@defproc[(string-ensure-ends-with-newline
           [s string?])
         string?]{

Ensures that @racket[s] ends with @racket[#\newline], adding one when needed.

@examples[
  #:eval st-eval
  (string-ensure-ends-with-newline "")
  (string-ensure-ends-with-newline "abc")
  (string-ensure-ends-with-newline "abc\n")
]
}

@defproc[(string-map
           [proc (-> char? char?)]
           [s string?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         string?]{

Applies @racket[proc] to each character in @racket[s] from @racket[start] to
@racket[end] and returns the resulting string.

This procedure does not mutate @racket[s].

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-map char-upcase "abc")
  (string-map char-upcase "abcdef" 1 4)
  (string-map char-upcase "abcdef" -4 -1)
]
}

@defproc[(string-map!
           [proc (-> char? char?)]
           [s (and/c string? (not/c immutable?))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)])
         void?]{

Applies @racket[proc] to each character in @racket[s] from @racket[start] to
@racket[end], mutating @racket[s] in place.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (define m (string-copy "abcdef"))
  (string-map! char-upcase m -4 -1)
  m
]
}

@defproc[(string-intersperse
           [sep string?]
           [xs (listof string?)])
         string?]{

Joins the strings in @racket[xs], inserting @racket[sep] between consecutive
elements.

@examples[
  #:eval st-eval
  (string-intersperse "," '())
  (string-intersperse "," '("a"))
  (string-intersperse "," '("a" "b" "c"))
]
}

@subsection{Escaping and Cleaning}

This subsection groups escaping and cleanup utilities for both human-visible
text and machine-oriented string formats such as quoted literals and JSON
string content.

@defproc[(string-escape-visible
           [s string?])
         string?]{

Returns a display-oriented escaped version of @racket[s], where common control
characters are replaced by visible escape sequences.

Escapes include @racket["\\n"], @racket["\\r"], @racket["\\t"],
@racket["\\b"], @racket["\\f"], and @racket["\\\\"]. Other ASCII control
characters are rendered as @racket["\\xNN"].

@examples[
  #:eval st-eval
  (string-escape-visible "\n\t\r")
  (string-escape-visible "\\x")
  (string-escape-visible (string #\nul #\rubout))
  (string-unescape-visible (string-escape-visible "a\n\tb"))
]
}

@defproc[(string-unescape-visible
           [s string?])
         string?]{

Parses visible escape sequences in @racket[s] and returns the corresponding
string with escaped characters restored.

Recognized escapes include @racket["\\n"], @racket["\\r"], @racket["\\t"],
@racket["\\b"], @racket["\\f"], @racket["\\\\"], @racket["\\xNN"], and
@racket["\\x...;"].

@examples[
  #:eval st-eval
  (string-unescape-visible "\\n\\t")
  (string-unescape-visible "\\x00\\x7F")
  (string-unescape-visible "\\x3BB;")
  (string-unescape-visible (string-escape-visible "a\n\tb"))
]

Related: @racket[string-quote], @racket[string-unquote].
}

@defproc[(string-quote
           [s string?]
           [#:quote-char quote-char char? #\"])
         string?]{

Wraps @racket[s] in quotes and escapes embedded quote, backslash, and common
control characters.

@examples[
  #:eval st-eval
  (string-quote "He said \"hi\"")
  (string-quote "a'b" #:quote-char #\')
  (string-unquote (string-quote "a\nb"))
]

Related: @racket[string-unquote], @racket[string-escape-visible], @racket[string-escape-json].
}

@defproc[(string-unquote
           [s string?]
           [#:quote-char quote-char char? #\"])
         string?]{

Removes matching outer quotes from @racket[s] and unescapes the quoted content.

An exception is raised when outer quotes are missing or escapes are malformed.

@examples[
  #:eval st-eval
  (string-unquote "\"a\\nb\"")
  (string-unquote "'a\\'b'" #:quote-char #\')
  (string-unquote (string-quote "He said \"hi\""))
]

Related: @racket[string-quote], @racket[string-unescape-visible], @racket[string-unescape-json].
}

@defproc[(string-escape-regexp
           [s string?])
         string?]{

Escapes @racket[s] so it can be used as a literal regular-expression pattern.

@examples[
  #:eval st-eval
  (string-escape-regexp "a+b")
  (regexp-match? (regexp (string-escape-regexp "a+b")) "a+b")
]
}

@defproc[(string-escape-json
           [s string?])
         string?]{

Escapes @racket[s] as JSON string content, without adding outer quotes.

@examples[
  #:eval st-eval
  (string-escape-json "\"\\/\n")
  (string-escape-json (string #\nul #\u001F))
  (string-unescape-json (string-escape-json "hello\nλ"))
]

Related: @racket[string-unescape-json], @racket[string-quote].
}

@defproc[(string-unescape-json
           [s string?])
         string?]{

Unescapes JSON string content, including @racket["\\uXXXX"] escapes and
surrogate pairs.

@examples[
  #:eval st-eval
  (string-unescape-json "\\u0041\\u03BB")
  (string-unescape-json "\\uD83D\\uDE00")
  (string-unescape-json (string-escape-json "hello\nλ"))
]

Related: @racket[string-escape-json], @racket[string-unquote].
}

@defproc[(string-strip-ansi
           [s string?])
         string?]{

Removes common terminal ANSI/VT control sequences from @racket[s], including
color/style control sequences and OSC metadata sequences.

@examples[
  #:eval st-eval
  (string-strip-ansi "\u001b[31mred\u001b[0m")
  (string-strip-ansi "a\u001b]0;title\u0007b")
]
}

@defproc[(string-squeeze
           [s string?]
           [to-squeeze (or/c char? char-set? string? (procedure-arity-includes/c 1))
                       char-whitespace?])
         string?]{

Collapses consecutive matching characters in @racket[s] into a single
character.

If @racket[to-squeeze] is a character, characters equal to it are squeezed. If
it is a character set, characters in the set are squeezed. If it is a string,
the string is treated as a character set. If it is a procedure, it is used as
the character predicate.

@examples[
  #:eval st-eval
  (string-squeeze "a   b    c" #\space)
  (string-squeeze "a\t \n\nb")
  (string-squeeze "baaaana" "a")
]
}

@subsection{Tokenization and Scanning}

This subsection groups parsing-oriented helpers that split text into tokens or
fields and scan text for successive match ranges.

@defproc[(string-tokenize
           [s string?]
           [to-separate (or/c char? char-set? string? (procedure-arity-includes/c 1))
                        char-whitespace?]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)]
           [#:quote quote (or/c #f char?) #f]
           [#:escape escape (or/c #f char?) #\\])
         (listof string?)]{

Splits @racket[s] into non-empty tokens using @racket[to-separate] as a
character matcher.

If @racket[quote] is provided, separators inside quoted text are ignored. If
@racket[escape] is provided, the escaped next character is treated literally.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-tokenize "  a  b   c  ")
  (string-tokenize "a,b,c" #\,)
  (string-tokenize "a,\"b,c\",d" #\, #:quote #\")
  (string-tokenize "a,b\\,c,d" #\, #:escape #\\)
  (string-tokenize "abc def ghi" char-whitespace? -7 -1)
]
}

@defproc[(string-fields
           [s string?]
           [to-separate (or/c char? char-set? string? (procedure-arity-includes/c 1))
                        #\,]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)]
           [#:quote quote (or/c #f char?) #f]
           [#:escape escape (or/c #f char?) #\\]
           [#:widths widths (or/c #f (listof exact-positive-integer?)) #f]
           [#:include-rest? include-rest? boolean? #f])
         (listof string?)]{

Splits @racket[s] into fields.

In delimiter mode, empty fields are preserved. In fixed-width mode
(@racket[#:widths]), the widths list defines field lengths from left to right.
When @racket[#:include-rest?] is true in fixed-width mode, one additional field
contains any remaining substring.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (string-fields "a,b,,c," #\,)
  (string-fields "a,\"b,c\",d" #\, #:quote #\")
  (string-fields "abcdefgh" #\, #:widths '(2 3 2))
  (string-fields "abcdefgh" #\, #:widths '(2 3 2) #:include-rest? #t)
  (string-fields "a,b,c,d" #\, -5 -1)
]
}

@defproc[(string-scan
           [s string?]
           [matcher (or/c string? char? char-set? (procedure-arity-includes/c 1))]
           [start exact-integer? 0]
           [end exact-integer? (string-length s)]
           [#:overlap? overlap? boolean? #f])
         (-> (or/c (cons/c exact-nonnegative-integer?
                           exact-nonnegative-integer?)
                    #f))]{

Returns a generator that produces successive match ranges as
@racket[(cons start end)] pairs, and then returns @racket[#f].

If @racket[matcher] is a string, it is treated as a substring needle.
If @racket[matcher] is a character, character set, or predicate, matching
characters are returned as one-character ranges.

Indices may be negative and are clamped to the string bounds.

@examples[
  #:eval st-eval
  (define (collect-ranges g)
    (let loop ([acc '()])
      (define v (g))
      (if v (loop (cons v acc)) (reverse acc))))
  (collect-ranges (string-scan "banana" "na"))
  (collect-ranges (string-scan "aaaa" "aa" #:overlap? #t))
  (collect-ranges (string-scan "abc123x" char-numeric?))
  (collect-ranges (string-scan "banana" #\a -5 -1))
]

Related: @racket[string-find-all-needle], @racket[string-find-needle].
}

@subsection{Formatting and Layout}

This subsection covers presentation-oriented text shaping, including wrapping,
indentation normalization, and width-constrained truncation.

@defproc[(string-wrap
           [s string?]
           [width exact-positive-integer?]
           [#:mode mode (or/c 'soft 'hard) 'soft]
           [#:preserve-words? preserve-words? boolean? #t])
         string?]{

Wraps @racket[s] to the requested line width.

In @racket['soft] mode, wrapping prefers whitespace boundaries. In
@racket['hard] mode, lines are split exactly at @racket[width] characters.

When @racket[preserve-words?] is true in soft mode, long words are kept intact
instead of being split.

@examples[
  #:eval st-eval
  (string-wrap "alpha beta gamma" 10)
  (string-wrap "supercalifragilistic" 8)
  (string-wrap "supercalifragilistic" 8 #:preserve-words? #f)
  (string-wrap "abcdefghij" 4 #:mode 'hard)
]
}

@defproc[(string-indent
           [s string?]
           [n-or-prefix (or/c exact-nonnegative-integer? string?)])
         string?]{

Indents every line in @racket[s].

If @racket[n-or-prefix] is a nonnegative integer, that many spaces are used.
If it is a string, that string is used as the line prefix.

@examples[
  #:eval st-eval
  (string-indent "a\nb" 2)
  (string-indent "a\nb" "-> ")
]
}

@defproc[(string-dedent
           [s string?])
         string?]{

Removes common leading indentation from all non-blank lines in @racket[s].

Indentation is measured using leading spaces and tabs.

@examples[
  #:eval st-eval
  (string-dedent "  a\n  b")
  (string-dedent "    a\n      b")
  (string-dedent "  a\n\n  b")
]
}

@defproc[(string-elide
           [s string?]
           [width exact-nonnegative-integer?]
           [#:where where (or/c 'left 'right 'middle) 'right]
           [#:ellipsis ellipsis string? "..."])
         string?]{

Truncates @racket[s] to at most @racket[width] characters by inserting
@racket[ellipsis].

The @racket[where] option chooses whether truncation happens on the left, right,
or in the middle.

@examples[
  #:eval st-eval
  (string-elide "abcdef" 5)
  (string-elide "abcdef" 5 #:where 'left)
  (string-elide "abcdef" 5 #:where 'middle)
  (string-elide "abcdef" 6 #:ellipsis "..")
]
}

@subsection{Similarity and Distance}

This subsection provides string similarity and distance metrics useful for
ranking candidates, fuzzy matching, and suggestion-style diagnostics.

@margin-note{🌐 See
@hyperlink["https://en.wikipedia.org/wiki/Levenshtein_distance"]{Levenshtein distance on Wikipedia}.}
@defproc[(string-levenshtein
           [a string?]
           [b string?])
         exact-nonnegative-integer?]{

Computes the Levenshtein edit distance between @racket[a] and @racket[b].
The result is the minimum number of insertions, deletions, and substitutions
needed to transform one string into the other.

Time complexity is @racket[O((string-length a) * (string-length b))].

@examples[
  #:eval st-eval
  (string-levenshtein "kitten" "sitting")
  (string-levenshtein "flaw" "lawn")
  (string-levenshtein "abc" "abc")
]
}

@margin-note{🌐 See
@hyperlink["https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance"]{Jaro-Winkler distance on Wikipedia}.}
@defproc[(string-jaro-winkler
           [a string?]
           [b string?]
           [#:prefix-scale prefix-scale real? 0.1])
         inexact-real?]{

Computes the Jaro-Winkler similarity score between @racket[a] and @racket[b].
Scores are in the range from @racket[0.0] to @racket[1.0], where larger values
indicate stronger similarity.
The @racket[prefix-scale] value must be between @racket[0.0] and
@racket[0.25].

Time complexity is approximately @racket[O((string-length a) * (string-length b))]
in the worst case.

@examples[
  #:eval st-eval
  (string-jaro-winkler "martha" "marhta")
  (string-jaro-winkler "martha" "xyz")
]
}

@defproc[(string-similarity
           [a string?]
           [b string?])
         inexact-real?]{

Returns a suggestion-oriented similarity score between @racket[a] and
@racket[b]. This is currently an alias of @racket[string-jaro-winkler].

@examples[
  #:eval st-eval
  (string-similarity "dixon" "dicksonx")
]
}

@subsection{Case Conversion and Predicates}

This subsection provides lightweight whole-string predicates for whitespace,
ASCII, and digit checks.

@defproc[(string-blank?
           [s string?])
         boolean?]{

Returns @racket[#t] if every character in @racket[s] is whitespace.

@examples[
  #:eval st-eval
  (string-blank? "")
  (string-blank? " \t\n")
  (string-blank? " a ")
]
}

@defproc[(string-ascii?
           [s string?])
         boolean?]{

Returns @racket[#t] if every character in @racket[s] is an ASCII character.

@examples[
  #:eval st-eval
  (string-ascii? "")
  (string-ascii? "ABC123!?")
  (string-ascii? "café")
]
}

@defproc[(string-digit?
           [s string?])
         boolean?]{

Returns @racket[#t] if every character in @racket[s] is an ASCII digit
(@racket[#\0] through @racket[#\9]).

@examples[
  #:eval st-eval
  (string-digit? "")
  (string-digit? "0123456789")
  (string-digit? "12a3")
]
}

@section{Character Sets}

This section documents the character-set utilities used by
@racket[string-count] and available directly through @racketmodname[string-tools/char-set].

Conceptually, a character set represents a collection of characters with
membership operations and set operations such as union, intersection, and
difference. It is useful when you want to classify characters efficiently and
reuse that classification across multiple string-processing steps.

Character sets are represented with a hybrid structure:
an ASCII bit mask for codepoints @racket[0] through @racket[127], plus a
normalized collection of non-ASCII inclusive ranges. This representation gives
fast membership tests for common ASCII text while keeping non-ASCII sets
compact.

@defmodule[string-tools/char-set]

@defproc[(char-set? [v any/c]) boolean?]{
Returns @racket[#t] when @racket[v] is a character set value.

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (char-set? (make-char-set #\a #\b))
  (char-set? "ab")
]
}

@defthing[empty-char-set char-set?]{
The empty character set.

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (char-set-size empty-char-set)
  (char-set-member? empty-char-set #\a)
]
}

@defproc[(make-char-set [ch char?] ...) char-set?]{
Builds a character set containing the given characters.

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (make-char-set #\a #\b #\a)
]
}

@defproc[(list->char-set [xs (listof char?)]) char-set?]{
Builds a character set from a list of characters.

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define cs (list->char-set (list #\a #\b #\a)))
  (char-set-size cs)
  (char-set-member? cs #\b)
]
}

@defproc[(string->char-set [s string?]) char-set?]{
Builds a character set from the distinct characters in @racket[s].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define cs (string->char-set "banana"))
  (char-set-size cs)
  (char-set-member? cs #\n)
]
}

@defproc[(char-set-add [cs char-set?] [ch char?]) char-set?]{
Returns a character set containing all characters in @racket[cs] and
@racket[ch].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define cs (char-set-add empty-char-set #\x))
  (char-set-member? cs #\x)
]
}

@defproc[(char-set-add-range [cs char-set?] [lo-ch char?] [hi-ch char?]) char-set?]{
Returns a character set containing @racket[cs] plus all characters from
@racket[lo-ch] to @racket[hi-ch], inclusive.

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define letters (char-set-add-range empty-char-set #\a #\z))
  (char-set-member? letters #\m)
  (char-set-member? letters #\A)
]
}

@defproc[(char-set-member? [cs char-set?] [ch char?]) boolean?]{
Checks whether @racket[ch] is in @racket[cs].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define vowels (make-char-set #\a #\e #\i #\o #\u))
  (char-set-member? vowels #\e)
  (char-set-member? vowels #\y)
]
}

@defproc[(char-set-union [a char-set?] [b char-set?]) char-set?]{
Returns the union of @racket[a] and @racket[b].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define vowels (make-char-set #\a #\e #\i #\o #\u))
  (define y      (make-char-set #\y))
  (char-set-member? (char-set-union vowels y) #\y)
]
}

@defproc[(char-set-intersection [a char-set?] [b char-set?]) char-set?]{
Returns the intersection of @racket[a] and @racket[b].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define a (make-char-set #\a #\b #\c))
  (define b (make-char-set #\b #\c #\d))
  (char-set-size (char-set-intersection a b))
]
}

@defproc[(char-set-difference [a char-set?] [b char-set?]) char-set?]{
Returns the set difference @racket[a - b].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define letters (char-set-add-range empty-char-set #\a #\f))
  (define vowels  (make-char-set #\a #\e))
  (char-set-member? (char-set-difference letters vowels) #\b)
  (char-set-member? (char-set-difference letters vowels) #\a)
]
}

@defproc[(char-set-size [cs char-set?]) exact-nonnegative-integer?]{
Returns the number of characters in @racket[cs].

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (char-set-size (make-char-set #\a #\b #\a))
]
}

@examples[
  #:eval st-eval
  (require string-tools/char-set)
  (define vowels (make-char-set #\a #\e #\i #\o #\u))
  (char-set-member? vowels #\e)
  (char-set-size vowels)
  (define letters (char-set-add-range empty-char-set #\a #\z))
  (char-set-size (char-set-difference letters vowels))
]
