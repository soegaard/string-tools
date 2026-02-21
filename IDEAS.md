Nice — if you’re curating a **practical, Python-informed string layer for Racket**, there’s a clear set of *high-leverage* additions that feel missing even to experienced Racketeers.

Below is a **curated list**, grouped by theme, with notes on *why* each one earns its place.

I’ll stay in **Racket-idiomatic function style**, not Python-method style.

---

## 1. Counting & occurrence utilities (high value)

You already added the big one 👍

### ✅ `string-count-needle` (you have this)

* Non-overlapping substring count
* Python parity

### 🔹 `string-count-char`

```racket
(string-count-char s ch)
```

* Faster than regex or `for`
* Common for delimiters, whitespace, validation
* Clearer than `(for/sum ...)`

---

## 2. Search helpers that return *indices* (not booleans)

Racket has prefix/suffix predicates, but not enough *index-level* helpers.

### 🔹 `string-index`

```racket
(string-index s ch [start])
```

* First occurrence or `#f`
* Equivalent to Python `find` for chars

### 🔹 `string-rindex`

```racket
(string-rindex s ch)
```

* Last occurrence
* Very common in filename/path parsing

### 🔹 `string-find-needle`

```racket
(string-find-needle s sub [start])
```

* Substring search → index or `#f`
* Clearer than regex for simple tasks

---

## 3. Strip / trim variants (surprisingly useful)

Racket’s `string-trim` is good but incomplete.

### 🔹 `string-trim-left`

### 🔹 `string-trim-right`

```racket
(string-trim-left s)
(string-trim-right s)
```

* Python parity (`lstrip`, `rstrip`)
* Often needed independently (parsers!)

Optional:

```racket
(string-trim-left s char-set)
```

---

## 4. Case convenience functions

Racket has powerful primitives, but misses *ergonomic* helpers.

### 🔹 `string-capitalize`

```racket
(string-capitalize "hello world") ; => "Hello world"
```

Very common in UIs and error messages.

### 🔹 `string-swapcase`

```racket
(string-swapcase "AbC") ; => "aBc"
```

Simple, obvious, absent.

---

## 5. Predicate helpers (clarity > power)

Python-style tests are popular because they read well.

### 🔹 `string-blank?`

```racket
(string-blank? s) ; only whitespace?
```

You *can* write this every time — but you shouldn’t have to.

### 🔹 `string-ascii?`

```racket
(string-ascii? s)
```

Useful for validation, encoders, ports, protocols.

### 🔹 `string-digit?`

```racket
(string-digit? s)
```

All characters are `0–9`

---

## 6. Partitioning / splitting helpers

### 🔹 `string-partition`

```racket
(string-partition s needle)
;; -> (values before needle after)
```

Python:

```python
s.partition(":")
```

This is *shockingly useful* for parsers and configs.

### 🔹 `string-rpartition`

Same, but from the right.

---

## 7. Safe slicing helpers (no exceptions)

### 🔹 `string-slice`

```racket
(string-slice s start end)
```

* Clamps indices
* Never raises bounds errors
* Matches Python slicing intuition

This is a *huge* ergonomics win.

---

## 8. Line-oriented helpers

### 🔹 `string-lines`

```racket
(string-lines s)
```

* Split on `\n`, `\r\n`, `\r`
* Optionally preserve line endings

Python:

```python
s.splitlines()
```

Racket has regex solutions — but this deserves a name.

---

## 9. Join / intersperse variants

### 🔹 `string-intersperse`

```racket
(string-intersperse "," '("a" "b" "c"))
```

Cleaner than:

```racket
(string-join xs ",")
```

when the separator is conceptually primary.

---

## 10. Debug / display helpers

Since you’re working close to the runtime…

### 🔹 `string-escape-visible`

```racket
(string-escape-visible "\n\t")
;; => "\\n\\t"
```

* Great for error messages
* Distinct from Racket reader escapes

---

## Minimal “core extras” recommendation

If you wanted a **small but powerful** extension set:

```text
string-count-needle
string-count-char
string-find-needle
string-index / string-rindex
string-trim-left / string-trim-right
string-capitalize
string-blank?
string-partition / string-rpartition
string-slice
string-lines
```
