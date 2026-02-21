#lang racket/base

(require racket/list
         racket/generator
         racket/string
         "char-set.rkt")

(provide string-split-at
         string-count-needle
         string-count
         string-find-needle
         string-find-last-needle
         string-find-all-needle
         string-partition
         string-partition-right
         string-replace-range
         string-repeat
         string-reverse
         string-levenshtein
         string-jaro-winkler
         string-similarity
         string-wrap
         string-indent
         string-dedent
         string-elide
         string-common-prefix
         string-common-suffix
         string-between
         string-slice
         string-slice/step
         string-at
         string-lines
         string-count-lines
         string-line-start-indices
         string-normalize-newlines
         string-expand-tabs
         string-display-width
         string-chomp
         string-chop-newline
         string-capitalize
         string-swapcase
         string-rot13
         string-pluralize
         string-singularize
         string-ensure-ends-with-newline
         string-map
         string-map!
         string-blank?
         string-ascii?
         string-digit?
         string-intersperse
         string-quote
         string-unquote
         string-escape-regexp
         string-escape-visible
         string-unescape-visible
         string-escape-json
         string-unescape-json
         string-strip-ansi
         string-squeeze
         string-tokenize
         string-fields
         string-scan
         string-remove-prefix
         string-remove-suffix
         string-ensure-prefix
         string-ensure-suffix
         string-trim-left
         string-trim-right
         string-index
         string-index-right
         string-skip
         string-skip-right)

;; string-split-at : string? exact-integer? ... -> (listof string?)
;;   Split a string at the given indices and return the substrings.
(define (string-split-at s . is)
  (define n (string-length s))
  (define (normalize-index i)
    (unless (exact-integer? i)
      (raise-argument-error
       'string-split-at
       "indices must be exact integers"
       is))
    (normalize-bound n i))
  (cond
    [(null? is)
     (list s)]
    [(null? (cdr is))
     (define i (normalize-index (car is)))
     (list (substring s 0 i)
           (substring s i n))]
    [else
     (define user-cuts (remove-duplicates (sort (map normalize-index is) <)))
     (define cuts (append (list 0) user-cuts (list n)))
     (for/list ([i (in-list cuts)]
                [j (in-list (cdr cuts))])
       (substring s i j))]))

;; normalize-bound : exact-nonnegative-integer? exact-integer? -> exact-nonnegative-integer?
;;   Normalize a possibly-negative index and clamp it into [0, n].
(define (normalize-bound n i)
  (define i0 (if (negative? i) (+ n i) i))
  (min n (max 0 i0)))

;; normalize-start/end : symbol? exact-nonnegative-integer? any/c any/c
;;                       -> (values exact-nonnegative-integer? exact-nonnegative-integer?)
;;   Normalize and clamp two bounds using string-slice conventions.
(define (normalize-start/end who n start end)
  (unless (exact-integer? start)
    (raise-argument-error who "exact-integer?" start))
  (unless (exact-integer? end)
    (raise-argument-error who "exact-integer?" end))
  (values (normalize-bound n start)
          (normalize-bound n end)))

;; string-count-needle : string? string? [exact-integer?]
;;                      [exact-integer?] -> exact-nonnegative-integer?
;;   Count non-overlapping occurrences of needle in s, like Python's str.count.
(define (string-count-needle s needle [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-count-needle "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-count-needle "string?" needle))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-count-needle n start end))
  (define m (string-length needle))

  (cond
    [(= m 0)               (if (<= start* end*) (+ 1 (- end* start*)) 0)]
    [(> m (- end* start*)) 0]
    [else
     (define (matches-at? i)
       (for/and ([k (in-range m)])
         (char=? (string-ref s (+ i k))
                 (string-ref needle k))))
     (let loop ([i start*] [k 0])
       (cond
         [(> (+ i m) end*) k]
         [(matches-at? i) (loop (+ i m) (add1 k))]
         [else            (loop (add1 i) k)]))]))

;; string-find-needle : string? string? [exact-integer?]
;;                      [exact-integer?]
;;                      -> (or/c exact-nonnegative-integer? #f)
;;   Find the first index where needle occurs in s from start to end.
(define (string-find-needle s needle [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-find-needle "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-find-needle "string?" needle))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-find-needle n start end))
  (define m (string-length needle))

  (define (matches-at? i)
    (for/and ([k (in-range m)])
      (char=? (string-ref s (+ i k))
              (string-ref needle k))))

  (cond
    [(= m 0)                 (and (<= start* end*) start*)]
    [(> m (- end* start*))   #f]
    [else
     (let loop ([i start*])
       (cond
         [(> (+ i m) end*)   #f]
         [(matches-at? i)     i]
         [else                (loop (add1 i))]))]))

;; string-find-last-needle : string? string? [exact-integer?]
;;                           [exact-integer?]
;;                           -> (or/c exact-nonnegative-integer? #f)
;;   Find the last index where needle occurs in s from start to end.
(define (string-find-last-needle s needle [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-find-last-needle "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-find-last-needle "string?" needle))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-find-last-needle n start end))
  (define m (string-length needle))

  (define (matches-at? i)
    (for/and ([k (in-range m)])
      (char=? (string-ref s (+ i k))
              (string-ref needle k))))

  (cond
    [(= m 0)                 (and (<= start* end*) end*)]
    [(> m (- end* start*))   #f]
    [else
     (let loop ([i (- end* m)])
       (cond
         [(< i start*)       #f]
         [(matches-at? i)     i]
         [else                (loop (sub1 i))]))]))

;; string-find-all-needle : string? string? [exact-integer?]
;;                          [exact-integer?]
;;                          [#:overlap? boolean?]
;;                          [#:ranges? boolean?]
;;                          -> (listof (or/c exact-nonnegative-integer?
;;                                           (cons/c exact-nonnegative-integer?
;;                                                   exact-nonnegative-integer?)))
;;   Find all occurrences of needle and return indices (or ranges) in selected bounds.
(define (string-find-all-needle s
                                needle
                                [start 0]
                                [end (string-length s)]
                                #:overlap? [overlap? #f]
                                #:ranges? [ranges? #f])
  (unless (string? s)
    (raise-argument-error 'string-find-all-needle "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-find-all-needle "string?" needle))
  (unless (boolean? overlap?)
    (raise-argument-error 'string-find-all-needle "boolean?" overlap?))
  (unless (boolean? ranges?)
    (raise-argument-error 'string-find-all-needle "boolean?" ranges?))

  (define n (string-length s))
  (define m (string-length needle))
  (define-values (start* end*) (normalize-start/end 'string-find-all-needle n start end))
  (define (emit i)
    (if ranges?
        (cons i (+ i m))
        i))

  (cond
    [(> start* end*)
     '()]
    [(zero? m)
     (for/list ([i (in-range start* (add1 end*))])
       (if ranges?
           (cons i i)
           i))]
    [else
     (let loop ([i start*] [acc '()])
       (define j (string-find-needle s needle i end*))
       (if j
           (loop (+ j (if overlap? 1 m))
                 (cons (emit j) acc))
           (reverse acc)))]))

;; string-partition : string? string? [exact-integer?]
;;                    [exact-integer?] -> (values string? string? string?)
;;   Partition s at the first occurrence of needle in the selected substring.
(define (string-partition s needle [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-partition "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-partition "string?" needle))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-partition n start end))
  (define i (string-find-needle s needle start* end*))
  (if i
      (values (substring s start* i)
              needle
              (substring s (+ i (string-length needle)) end*))
      (values (if (<= start* end*) (substring s start* end*) "")
              ""
              "")))

;; string-partition-right : string? string? [exact-integer?]
;;                          [exact-integer?] -> (values string? string? string?)
;;   Partition s at the last occurrence of needle in the selected substring.
(define (string-partition-right s needle [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-partition-right "string?" s))
  (unless (string? needle)
    (raise-argument-error 'string-partition-right "string?" needle))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-partition-right n start end))
  (define i (string-find-last-needle s needle start* end*))
  (if i
      (values (substring s start* i)
              needle
              (substring s (+ i (string-length needle)) end*))
      (values (if (<= start* end*) (substring s start* end*) "")
              ""
              "")))

;; string-replace-range : string? exact-integer? exact-integer? string? -> string?
;;   Replace the selected start/end range in s with replacement.
(define (string-replace-range s start end replacement)
  (unless (string? s)
    (raise-argument-error 'string-replace-range "string?" s))
  (unless (string? replacement)
    (raise-argument-error 'string-replace-range "string?" replacement))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-replace-range n start end))
  (define from (min start* end*))
  (define to   (max start* end*))
  (string-append (substring s 0 from)
                 replacement
                 (substring s to n)))

;; string-repeat : string? exact-nonnegative-integer? -> string?
;;   Repeat s n times.
(define (string-repeat s n)
  (unless (string? s)
    (raise-argument-error 'string-repeat "string?" s))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'string-repeat "exact-nonnegative-integer?" n))
  (let loop ([k n] [chunk s] [acc ""])
    (cond
      [(zero? k) acc]
      [(odd? k)  (loop (quotient k 2) (string-append chunk chunk) (string-append acc chunk))]
      [else      (loop (quotient k 2) (string-append chunk chunk) acc)])))

;; string-reverse : string? -> string?
;;   Reverse characters in s.
(define (string-reverse s)
  (unless (string? s)
    (raise-argument-error 'string-reverse "string?" s))
  (list->string (reverse (string->list s))))

;; string-wrap : string? exact-positive-integer?
;;               [#:mode (or/c 'soft 'hard)]
;;               [#:preserve-words? boolean?]
;;               -> string?
;;   Wrap each line in s to width; soft mode prefers whitespace boundaries.
(define (string-wrap s width #:mode [mode 'soft] #:preserve-words? [preserve-words? #t])
  (unless (string? s)
    (raise-argument-error 'string-wrap "string?" s))
  (unless (exact-positive-integer? width)
    (raise-argument-error 'string-wrap "exact-positive-integer?" width))
  (unless (memq mode '(soft hard))
    (raise-argument-error 'string-wrap "(or/c 'soft 'hard)" mode))
  (unless (boolean? preserve-words?)
    (raise-argument-error 'string-wrap "boolean?" preserve-words?))

  (define n (string-length s))
  (define (space-index-right from to)
    (let loop ([i (sub1 to)])
      (cond
        [(< i from)                    #f]
        [(char-whitespace? (string-ref s i)) i]
        [else                          (loop (sub1 i))])))
  (define (space-index-left from)
    (let loop ([i from])
      (cond
        [(>= i n)                      #f]
        [(char-whitespace? (string-ref s i)) i]
        [else                          (loop (add1 i))])))
  (define (skip-spaces i)
    (let loop ([j i])
      (cond
        [(>= j n)                      n]
        [(char-whitespace? (string-ref s j)) (loop (add1 j))]
        [else                          j])))
  (cond
    ((zero? n)
     "")
    ((eq? mode 'hard)
     (string-join
      (for/list ([i (in-range 0 n width)])
        (substring s i (min n (+ i width))))
      "\n"))
    (else
     (let loop ([i 0] [acc '()])
       (cond
         ((>= i n)
          (string-join (reverse acc) "\n"))
         ((<= (- n i) width)
          (string-join (reverse (cons (substring s i n) acc)) "\n"))
         (else
          (define limit      (+ i width))
          (define last-space (space-index-right i limit))
          (cond
            (last-space
             (define line   (substring s i last-space))
             (define next-i (skip-spaces (add1 last-space)))
             (loop next-i (cons line acc)))
            (preserve-words?
             (define next-space (space-index-left limit))
             (cond
               (next-space
                (define line   (substring s i next-space))
                (define next-i (skip-spaces (add1 next-space)))
                (loop next-i (cons line acc)))
               (else
                (string-join (reverse (cons (substring s i n) acc)) "\n"))))
            (else
             (define line (substring s i limit))
             (loop limit (cons line acc))))))))))

;; split-nl : string? -> (listof string?)
;;   Split s at #\newline and retain a trailing empty line when s ends with #\newline.
(define (split-nl s)
  (define n (string-length s))
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(>= i n)
       (reverse (cons (substring s start n) acc))]
      [(char=? (string-ref s i) #\newline)
       (loop (add1 i) (add1 i) (cons (substring s start i) acc))]
      [else
       (loop (add1 i) start acc)])))

;; string-indent : string? (or/c exact-nonnegative-integer? string?) -> string?
;;   Indent every line in s by spaces or by a literal prefix.
(define (string-indent s n-or-prefix)
  (unless (string? s)
    (raise-argument-error 'string-indent "string?" s))
  (define prefix
    (cond
      [(exact-nonnegative-integer? n-or-prefix)
       (make-string n-or-prefix #\space)]
      [(string? n-or-prefix)
       n-or-prefix]
      [else
       (raise-argument-error 'string-indent
                             "(or/c exact-nonnegative-integer? string?)"
                             n-or-prefix)]))
  (define lines (split-nl s))
  (string-join (map (λ (line) (string-append prefix line)) lines) "\n"))

;; string-dedent : string? -> string?
;;   Remove common leading spaces/tabs from all non-blank lines in s.
(define (string-dedent s)
  (unless (string? s)
    (raise-argument-error 'string-dedent "string?" s))
  (define lines (split-nl s))
  (define (blank-line? line)
    (for/and ([ch (in-string line)])
      (or (char=? ch #\space) (char=? ch #\tab))))
  (define (leading-indent line)
    (let loop ([i 0] [n (string-length line)])
      (cond
        [(>= i n) i]
        [(or (char=? (string-ref line i) #\space)
             (char=? (string-ref line i) #\tab))
         (loop (add1 i) n)]
        [else i])))
  (define indents
    (for/list ([line (in-list lines)]
               #:unless (blank-line? line))
      (leading-indent line)))
  (define drop-n
    (if (null? indents)
        0
        (apply min indents)))
  (define dedented
    (for/list ([line (in-list lines)])
      (define m (string-length line))
      (substring line (min drop-n m) m)))
  (string-join dedented "\n"))

;; string-elide : string? exact-nonnegative-integer?
;;                [#:where (or/c 'left 'right 'middle)]
;;                [#:ellipsis string?]
;;                -> string?
;;   Truncate s to width using ellipsis on the left, right, or middle.
(define (string-elide s width #:where [where 'right] #:ellipsis [ellipsis "..."])
  (unless (string? s)
    (raise-argument-error 'string-elide "string?" s))
  (unless (exact-nonnegative-integer? width)
    (raise-argument-error 'string-elide "exact-nonnegative-integer?" width))
  (unless (memq where '(left right middle))
    (raise-argument-error 'string-elide "(or/c 'left 'right 'middle)" where))
  (unless (string? ellipsis)
    (raise-argument-error 'string-elide "string?" ellipsis))
  (define n (string-length s))
  (define e (string-length ellipsis))
  (cond
    [(<= n width)
     s]
    [(<= width e)
     (substring ellipsis 0 width)]
    [else
     (define keep (- width e))
     (cond
       [(eq? where 'right)
        (string-append (substring s 0 keep) ellipsis)]
       [(eq? where 'left)
        (string-append ellipsis (substring s (- n keep) n))]
       [else
        (define left-n  (quotient keep 2))
        (define right-n (- keep left-n))
        (string-append (substring s 0 left-n)
                       ellipsis
                       (substring s (- n right-n) n))])]))

;; string-common-prefix : string? string? -> string?
;;   Return the longest common prefix of a and b.
(define (string-common-prefix a b)
  (unless (string? a)
    (raise-argument-error 'string-common-prefix "string?" a))
  (unless (string? b)
    (raise-argument-error 'string-common-prefix "string?" b))
  (define na (string-length a))
  (define nb (string-length b))
  (define n (min na nb))
  (define k
    (let loop ([i 0])
      (cond
        [(>= i n) i]
        [(char=? (string-ref a i) (string-ref b i)) (loop (add1 i))]
        [else i])))
  (substring a 0 k))

;; string-common-suffix : string? string? -> string?
;;   Return the longest common suffix of a and b.
(define (string-common-suffix a b)
  (unless (string? a)
    (raise-argument-error 'string-common-suffix "string?" a))
  (unless (string? b)
    (raise-argument-error 'string-common-suffix "string?" b))
  (define na (string-length a))
  (define nb (string-length b))
  (define n (min na nb))
  (define k
    (let loop ([i 0])
      (cond
        [(>= i n) i]
        [(char=? (string-ref a (- na 1 i)) (string-ref b (- nb 1 i))) (loop (add1 i))]
        [else i])))
  (substring a (- na k) na))

;; string-levenshtein : string? string? -> exact-nonnegative-integer?
;;   Compute Levenshtein edit distance between a and b.
(define (string-levenshtein a b)
  (unless (string? a)
    (raise-argument-error 'string-levenshtein "string?" a))
  (unless (string? b)
    (raise-argument-error 'string-levenshtein "string?" b))
  (define na (string-length a))
  (define nb (string-length b))
  (cond
    [(zero? na) nb]
    [(zero? nb) na]
    [else
     (define vec-a
       (for/vector ([ch (in-string a)]) ch))
     (define vec-b
       (for/vector ([ch (in-string b)]) ch))
     (define prev (make-vector (add1 nb) 0))
     (define curr (make-vector (add1 nb) 0))
     (for ([j (in-range (add1 nb))])
       (vector-set! prev j j))
     (for ([i (in-range 1 (add1 na))])
       (vector-set! curr 0 i)
       (for ([j (in-range 1 (add1 nb))])
         (define cost
           (if (char=? (vector-ref vec-a (sub1 i))
                       (vector-ref vec-b (sub1 j)))
               0
               1))
         (define del (add1 (vector-ref prev j)))
         (define ins (add1 (vector-ref curr (sub1 j))))
         (define sub (+ (vector-ref prev (sub1 j)) cost))
         (vector-set! curr j (min del ins sub)))
       (for ([j (in-range (add1 nb))])
         (vector-set! prev j (vector-ref curr j))))
     (vector-ref prev nb)]))

;; string-jaro-winkler : string? string? [#:prefix-scale real?] -> inexact-real?
;;   Compute the Jaro-Winkler similarity score in [0, 1].
(define (string-jaro-winkler a b #:prefix-scale [prefix-scale 0.1])
  (unless (string? a)
    (raise-argument-error 'string-jaro-winkler "string?" a))
  (unless (string? b)
    (raise-argument-error 'string-jaro-winkler "string?" b))
  (unless (and (real? prefix-scale) (<= 0.0 prefix-scale 0.25))
    (raise-argument-error 'string-jaro-winkler "real? in [0, 0.25]" prefix-scale))
  (define na (string-length a))
  (define nb (string-length b))
  (cond
    [(and (zero? na) (zero? nb)) 1.0]
    [(or (zero? na) (zero? nb)) 0.0]
    [else
     (define match-dist (max 0 (sub1 (quotient (max na nb) 2))))
     (define a-match (make-vector na #f))
     (define b-match (make-vector nb #f))
     (define matches
       (let loop-i ([i 0] [k 0])
         (if (>= i na)
             k
             (let* ([lo (max 0 (- i match-dist))]
                    [hi (min nb (+ i match-dist 1))]
                    [ai (string-ref a i)])
               (let loop-j ([j lo])
                 (cond
                   [(>= j hi)
                    (loop-i (add1 i) k)]
                   [(and (not (vector-ref b-match j))
                         (char=? ai (string-ref b j)))
                    (vector-set! a-match i #t)
                    (vector-set! b-match j #t)
                    (loop-i (add1 i) (add1 k))]
                   [else
                    (loop-j (add1 j))]))))))
     (if (zero? matches)
         0.0
         (let* ([a-ms
                 (for/list ([i (in-range na)] #:when (vector-ref a-match i))
                   (string-ref a i))]
                [b-ms
                 (for/list ([j (in-range nb)] #:when (vector-ref b-match j))
                   (string-ref b j))]
                [transpositions
                 (for/sum ([ca (in-list a-ms)] [cb (in-list b-ms)])
                   (if (char=? ca cb) 0 1))]
                [m (exact->inexact matches)]
                [t (/ transpositions 2.0)]
                [jaro (/ (+ (/ m na)
                            (/ m nb)
                            (/ (- m t) m))
                         3.0)]
                [prefix
                 (let loop ([k 0])
                   (cond
                     [(or (>= k 4) (>= k na) (>= k nb)) k]
                     [(char=? (string-ref a k) (string-ref b k))
                      (loop (add1 k))]
                     [else k]))])
           (+ jaro (* prefix prefix-scale (- 1.0 jaro)))))]))

;; string-similarity : string? string? -> inexact-real?
;;   Alias of string-jaro-winkler for suggestion-style similarity scoring.
(define (string-similarity a b)
  (string-jaro-winkler a b))

;; string-between : string? (or/c char? string?) (or/c char? string?)
;;                  [exact-integer?] [exact-integer?]
;;                  #:left-match (or/c 'first 'last)
;;                  #:right-match (or/c 'first 'last)
;;                  #:include-left? boolean?
;;                  #:include-right? boolean?
;;                  -> (or/c string? #f)
;;   Return substring between left and right delimiters in selected bounds.
(define (string-between s left right
                        [start 0]
                        [end (string-length s)]
                        #:left-match [left-match 'first]
                        #:right-match [right-match 'first]
                        #:include-left? [include-left? #f]
                        #:include-right? [include-right? #f])
  (unless (string? s)
    (raise-argument-error 'string-between "string?" s))
  (unless (or (char? left) (string? left))
    (raise-argument-error 'string-between "(or/c char? string?)" left))
  (unless (or (char? right) (string? right))
    (raise-argument-error 'string-between "(or/c char? string?)" right))
  (unless (member left-match '(first last))
    (raise-argument-error 'string-between "(or/c 'first 'last)" left-match))
  (unless (member right-match '(first last))
    (raise-argument-error 'string-between "(or/c 'first 'last)" right-match))
  (unless (boolean? include-left?)
    (raise-argument-error 'string-between "boolean?" include-left?))
  (unless (boolean? include-right?)
    (raise-argument-error 'string-between "boolean?" include-right?))
  (define left*
    (if (char? left)
        (string left)
        left))
  (define right*
    (if (char? right)
        (string right)
        right))
  (unless (positive? (string-length left*))
    (raise-argument-error 'string-between "non-empty delimiter string" left))
  (unless (positive? (string-length right*))
    (raise-argument-error 'string-between "non-empty delimiter string" right))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-between n start end))
  (if (> start* end*)
      #f
      (let* ([i (if (eq? left-match 'first)
                    (string-find-needle s left* start* end*)
                    (string-find-last-needle s left* start* end*))])
        (if (not i)
            #f
            (let* ([after-left (+ i (string-length left*))]
                   [search-from (if include-left? i after-left)]
                   [j (if (eq? right-match 'first)
                          (string-find-needle s right* search-from end*)
                          (string-find-last-needle s right* search-from end*))])
              (if (not j)
                  #f
                  (let ([from (if include-left? i after-left)]
                        [to   (if include-right? (+ j (string-length right*)) j)])
                    (if (<= from to)
                        (substring s from to)
                        #f))))))))

;; string-slice : string? [exact-integer?] [exact-integer?] -> string?
;;   Return a clamped Python-style slice of s.
(define (string-slice s [start 0] [end (string-length s)])
  (unless (string? s)
    (raise-argument-error 'string-slice "string?" s))
  (unless (exact-integer? start)
    (raise-argument-error 'string-slice "exact-integer?" start))
  (unless (exact-integer? end)
    (raise-argument-error 'string-slice "exact-integer?" end))

  (define n (string-length s))
  (define s0 (if (negative? start) (+ n start) start))
  (define e0 (if (negative? end)   (+ n end)   end))
  (define s1 (min n (max 0 s0)))
  (define e1 (min n (max 0 e0)))
  (if (<= e1 s1)
      ""
      (substring s s1 e1)))

;; string-slice/step : string? [(or/c exact-integer? #f)] [(or/c exact-integer? #f)]
;;                     [exact-integer?] -> string?
;;   Return a clamped slice with an optional non-zero step.
(define (string-slice/step s [start #f] [end #f] [step 1])
  (unless (string? s)
    (raise-argument-error 'string-slice/step "string?" s))
  (unless (or (not start) (exact-integer? start))
    (raise-argument-error 'string-slice/step "(or/c #f exact-integer?)" start))
  (unless (or (not end) (exact-integer? end))
    (raise-argument-error 'string-slice/step "(or/c #f exact-integer?)" end))
  (unless (exact-integer? step)
    (raise-argument-error 'string-slice/step "exact-integer?" step))
  (when (zero? step)
    (raise-argument-error 'string-slice/step "non-zero exact integer" step))

  (define n (string-length s))
  (define step-pos? (positive? step))
  (define default-start (if step-pos? 0 (sub1 n)))
  (define default-end   (if step-pos? n -1))
  (define start-raw (if (not start) default-start start))
  (define end-raw   (if (not end) default-end end))
  (define start* (normalize-bound n start-raw))
  (define end*
    (if step-pos?
        (normalize-bound n end-raw)
        ;; For reverse slicing, omitted end means one slot before index 0.
        (if (not end)
            -1
            (let ([e0 (if (negative? end-raw) (+ n end-raw) end-raw)])
              (min n (max -1 e0))))))

  (list->string
   (if step-pos?
       (for/list ([i (in-range start* end* step)])
         (string-ref s i))
       (for/list ([i (in-range start* end* step)])
         (string-ref s i)))))

;; string-at : string? exact-integer? [any/c] -> any/c
;;   Safely access a character by index; clamp indices, and use default for empty strings.
(define (string-at s i [default #f])
  (unless (string? s)
    (raise-argument-error 'string-at "string?" s))
  (unless (exact-integer? i)
    (raise-argument-error 'string-at "exact-integer?" i))
  (define n (string-length s))
  (cond
    [(zero? n)
     default]
    [else
     (define i0 (if (negative? i) (+ n i) i))
     (define i* (min (sub1 n) (max 0 i0)))
     (string-ref s i*)]))

;; string-lines : string? -> (listof string?)
;;   Split s into lines using \n, \r\n, and \r as line separators.
(define (string-lines s)
  (unless (string? s)
    (raise-argument-error 'string-lines "string?" s))
  (define n (string-length s))
  (let loop ([i 0] [line-start 0] [acc '()])
    (cond
      [(>= i n)
       (if (< line-start n)
           (reverse (cons (substring s line-start n) acc))
           (reverse acc))]
      [else
       (define ch (string-ref s i))
       (cond
         [(char=? ch #\newline)
          (define line (substring s line-start i))
          (if (= i (sub1 n))
              (reverse (cons line acc))
              (loop (add1 i) (add1 i) (cons line acc)))]
         [(char=? ch #\return)
          (define next-i
            (if (and (< (add1 i) n)
                     (char=? (string-ref s (add1 i)) #\newline))
                (+ i 2)
                (add1 i)))
          (define line (substring s line-start i))
          (if (= next-i n)
              (reverse (cons line acc))
              (loop next-i next-i (cons line acc)))]
         [else
          (loop (add1 i) line-start acc)])])))

;; string-count-lines : string? -> exact-positive-integer?
;;   Count lines in s using CR, LF, and CRLF as newline sequences.
(define (string-count-lines s)
  (unless (string? s)
    (raise-argument-error 'string-count-lines "string?" s))
  (define n (string-length s))
  (let loop ([i 0] [k 1])
    (cond
      [(>= i n)
       k]
      [else
       (define ch (string-ref s i))
       (cond
         [(char=? ch #\newline)
          (loop (add1 i) (add1 k))]
         [(char=? ch #\return)
          (if (and (< (add1 i) n)
                   (char=? (string-ref s (add1 i)) #\newline))
              (loop (+ i 2) (add1 k))
              (loop (add1 i) (add1 k)))]
         [else
          (loop (add1 i) k)])])))

;; string-line-start-indices : string? -> (listof exact-nonnegative-integer?)
;;   Return a list of character offsets for each line start.
(define (string-line-start-indices s)
  (unless (string? s)
    (raise-argument-error 'string-line-start-indices "string?" s))
  (define n (string-length s))
  (reverse
   (let loop ([i 0] [acc (list 0)])
     (cond
       [(>= i n)
        acc]
       [else
        (define ch (string-ref s i))
        (cond
          [(char=? ch #\newline)
           (loop (add1 i) (cons (add1 i) acc))]
          [(char=? ch #\return)
           (define j
             (if (and (< (add1 i) n)
                      (char=? (string-ref s (add1 i)) #\newline))
                 (+ i 2)
                 (add1 i)))
           (loop j (cons j acc))]
          [else
           (loop (add1 i) acc)])]))))

;; string-normalize-newlines : string? -> string?
;;   Convert CRLF and CR newlines in s to LF.
(define (string-normalize-newlines s)
  (unless (string? s)
    (raise-argument-error 'string-normalize-newlines "string?" s))
  (define n (string-length s))
  (define out (open-output-string))
  (let loop ([i 0])
    (cond
      [(>= i n)
       (get-output-string out)]
      [else
       (define ch (string-ref s i))
       (cond
         [(char=? ch #\return)
          (write-char #\newline out)
          (if (and (< (add1 i) n)
                   (char=? (string-ref s (add1 i)) #\newline))
              (loop (+ i 2))
              (loop (add1 i)))]
         [else
         (write-char ch out)
          (loop (add1 i))])])))

;; next-tab-column : exact-nonnegative-integer? exact-positive-integer?
;;                   -> exact-positive-integer?
;;   Return the column at the next tab stop.
(define (next-tab-column col tab-width)
  (+ col (- tab-width (remainder col tab-width))))

;; string-expand-tabs : string? [#:tab-width exact-positive-integer?]
;;                      [#:start-column exact-nonnegative-integer?]
;;                      -> string?
;;   Replace tab characters with spaces according to tab stops.
(define (string-expand-tabs s #:tab-width [tab-width 8] #:start-column [start-column 0])
  (unless (string? s)
    (raise-argument-error 'string-expand-tabs "string?" s))
  (unless (exact-positive-integer? tab-width)
    (raise-argument-error 'string-expand-tabs "exact-positive-integer?" tab-width))
  (unless (exact-nonnegative-integer? start-column)
    (raise-argument-error 'string-expand-tabs "exact-nonnegative-integer?" start-column))
  (define out (open-output-string))
  (let loop ([i 0] [n (string-length s)] [col start-column])
    (cond
      [(>= i n)
       (get-output-string out)]
      [else
       (define ch (string-ref s i))
       (cond
         [(char=? ch #\tab)
          (define next-col (next-tab-column col tab-width))
          (display (make-string (- next-col col) #\space) out)
          (loop (add1 i) n next-col)]
         [(char=? ch #\newline)
          (write-char ch out)
          (loop (add1 i) n 0)]
         [(char=? ch #\return)
          (write-char ch out)
          (loop (add1 i) n 0)]
         [else
          (write-char ch out)
          (loop (add1 i) n (add1 col))])])))

;; string-display-width : string? [#:tab-width exact-positive-integer?]
;;                        [#:start-column exact-nonnegative-integer?]
;;                        -> exact-nonnegative-integer?
;;   Return a monospace display-width approximation for the final line.
(define (string-display-width s #:tab-width [tab-width 8] #:start-column [start-column 0])
  (unless (string? s)
    (raise-argument-error 'string-display-width "string?" s))
  (unless (exact-positive-integer? tab-width)
    (raise-argument-error 'string-display-width "exact-positive-integer?" tab-width))
  (unless (exact-nonnegative-integer? start-column)
    (raise-argument-error 'string-display-width "exact-nonnegative-integer?" start-column))
  (let loop ([i 0] [n (string-length s)] [col start-column])
    (cond
      [(>= i n)
       col]
      [else
       (define ch (string-ref s i))
       (define cp (char->integer ch))
       (cond
         [(char=? ch #\tab)
          (loop (add1 i) n (next-tab-column col tab-width))]
         [(char=? ch #\newline)
          (loop (add1 i) n 0)]
         [(char=? ch #\return)
          (loop (add1 i) n 0)]
         [(or (< cp 32) (= cp 127))
          (loop (add1 i) n col)]
         [else
          (loop (add1 i) n (add1 col))])])))

;; string-chomp : string? -> string?
;;   Drop one trailing newline: either \n or \r\n.
(define (string-chomp s)
  (unless (string? s)
    (raise-argument-error 'string-chomp "string?" s))
  (define n (string-length s))
  (cond
    [(zero? n) ""]
    [(char=? (string-ref s (sub1 n)) #\newline)
     (if (and (>= n 2)
              (char=? (string-ref s (- n 2)) #\return))
         (substring s 0 (- n 2))
         (substring s 0 (sub1 n)))]
    [else s]))

;; string-chop-newline : string? -> string?
;;   Alias of string-chomp.
(define (string-chop-newline s)
  (string-chomp s))

;; string-capitalize : string? -> string?
;;   Return s with the first character upcased and the remaining characters downcased.
(define (string-capitalize s)
  (unless (string? s)
    (raise-argument-error 'string-capitalize "string?" s))
  (define n (string-length s))
  (cond
    [(zero? n) ""]
    [else
     (string-append (string-upcase (substring s 0 1))
                    (string-downcase (substring s 1 n)))]))

;; string-swapcase : string? -> string?
;;   Return s with uppercase and lowercase characters swapped.
(define (string-swapcase s)
  (unless (string? s)
    (raise-argument-error 'string-swapcase "string?" s))
  (list->string
   (for/list ([ch (in-string s)])
     (cond
       [(char-upper-case? ch) (char-downcase ch)]
       [(char-lower-case? ch) (char-upcase ch)]
       [else                  ch]))))

;; string-rot13 : string? -> string?
;;   Apply ROT13 to ASCII letters in s.
(define (string-rot13 s)
  (unless (string? s)
    (raise-argument-error 'string-rot13 "string?" s))
  (define (rot13-char ch)
    (define cp (char->integer ch))
    (cond
      [(<= (char->integer #\a) cp (char->integer #\z))
       (integer->char
        (+ (char->integer #\a)
           (modulo (+ (- cp (char->integer #\a)) 13) 26)))]
      [(<= (char->integer #\A) cp (char->integer #\Z))
       (integer->char
        (+ (char->integer #\A)
           (modulo (+ (- cp (char->integer #\A)) 13) 26)))]
      [else
       ch]))
  (list->string (for/list ([ch (in-string s)]) (rot13-char ch))))

;; string-pluralize : string? -> string?
;;   Convert a singular English-ish noun to a plural using simple heuristics.
(define (string-pluralize s)
  (unless (string? s)
    (raise-argument-error 'string-pluralize "string?" s))
  (define n (string-length s))
  (define (ends-with? suf)
    (define m (string-length suf))
    (and (<= m n)
         (string=? (substring s (- n m) n) suf)))
  (define (vowel? ch)
    (member (char-downcase ch) '(#\a #\e #\i #\o #\u)))
  (cond
    [(zero? n)
     "s"]
    [(or (ends-with? "s")
         (ends-with? "x")
         (ends-with? "z")
         (ends-with? "ch")
         (ends-with? "sh"))
     (string-append s "es")]
    [(and (ends-with? "y")
          (>= n 2)
          (not (vowel? (string-ref s (- n 2)))))
     (string-append (substring s 0 (sub1 n)) "ies")]
    [(ends-with? "fe")
     (string-append (substring s 0 (- n 2)) "ves")]
    [(ends-with? "f")
     (string-append (substring s 0 (sub1 n)) "ves")]
    [else
     (string-append s "s")]))

;; string-singularize : string? -> string?
;;   Convert a plural English-ish noun to a singular using simple heuristics.
(define (string-singularize s)
  (unless (string? s)
    (raise-argument-error 'string-singularize "string?" s))
  (define n (string-length s))
  (define (ends-with? suf)
    (define m (string-length suf))
    (and (<= m n)
         (string=? (substring s (- n m) n) suf)))
  (cond
    [(<= n 1)
     s]
    [(and (ends-with? "ies") (> n 3))
     (string-append (substring s 0 (- n 3)) "y")]
    [(and (ends-with? "ves") (> n 3))
     (string-append (substring s 0 (- n 3)) "f")]
    [(or (and (ends-with? "ches") (> n 4))
         (and (ends-with? "shes") (> n 4))
         (and (ends-with? "xes") (> n 3))
         (and (ends-with? "zes") (> n 3))
         (and (ends-with? "ses") (> n 3)))
     (substring s 0 (- n 2))]
    [(and (ends-with? "s") (> n 1))
     (substring s 0 (sub1 n))]
    [else
     s]))

;; string-ensure-ends-with-newline : string? -> string?
;;   Ensure s ends with a newline character.
(define (string-ensure-ends-with-newline s)
  (unless (string? s)
    (raise-argument-error 'string-ensure-ends-with-newline "string?" s))
  (define n (string-length s))
  (if (and (> n 0)
           (char=? (string-ref s (sub1 n)) #\newline))
      s
      (string-append s "\n")))

;; apply-string-map-range! : symbol? (-> char? any/c) string?
;;                           exact-nonnegative-integer? exact-nonnegative-integer?
;;                           -> void?
;;   Apply proc to each character in s from start* to end* and mutate in place.
(define (apply-string-map-range! who proc s start* end*)
  (for ([i (in-range start* end*)])
    (define in-ch (string-ref s i))
    (define out-ch (proc in-ch))
    (unless (char? out-ch)
      (raise-arguments-error who
                             "procedure must return a character"
                             "input-char" in-ch
                             "result" out-ch
                             "index" i))
    (string-set! s i out-ch)))

;; string-map : (-> char? char?) string? [exact-integer?] [exact-integer?] -> string?
;;   Return a new string with proc mapped over the selected range.
(define (string-map proc s [start 0] [end (string-length s)])
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-argument-error 'string-map "(procedure-arity-includes/c 1)" proc))
  (unless (string? s)
    (raise-argument-error 'string-map "string?" s))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-map n start end))
  (define from (min start* end*))
  (define to   (max start* end*))
  (define out  (string-copy s))
  (apply-string-map-range! 'string-map proc out from to)
  out)

;; string-map! : (-> char? char?) string? [exact-integer?] [exact-integer?] -> void?
;;   Map proc over the selected range of s, mutating s in place.
(define (string-map! proc s [start 0] [end (string-length s)])
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-argument-error 'string-map! "(procedure-arity-includes/c 1)" proc))
  (unless (string? s)
    (raise-argument-error 'string-map! "string?" s))
  (unless (not (immutable? s))
    (raise-argument-error 'string-map! "(and/c string? (not/c immutable?))" s))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-map! n start end))
  (define from (min start* end*))
  (define to   (max start* end*))
  (apply-string-map-range! 'string-map! proc s from to)
  (void))

;; string-blank? : string? -> boolean?
;;   Check whether every character in s is whitespace.
(define (string-blank? s)
  (unless (string? s)
    (raise-argument-error 'string-blank? "string?" s))
  (for/and ([ch (in-string s)])
    (char-whitespace? ch)))

;; string-ascii? : string? -> boolean?
;;   Check whether every character in s is an ASCII character.
(define (string-ascii? s)
  (unless (string? s)
    (raise-argument-error 'string-ascii? "string?" s))
  (for/and ([ch (in-string s)])
    (<= (char->integer ch) #x7F)))

;; string-digit? : string? -> boolean?
;;   Check whether every character in s is an ASCII digit.
(define (string-digit? s)
  (unless (string? s)
    (raise-argument-error 'string-digit? "string?" s))
  (for/and ([ch (in-string s)])
    (char<=? #\0 ch #\9)))

;; string-intersperse : string? (listof string?) -> string?
;;   Join strings with sep inserted between consecutive elements.
(define (string-intersperse sep xs)
  (unless (string? sep)
    (raise-argument-error 'string-intersperse "string?" sep))
  (unless (list? xs)
    (raise-argument-error 'string-intersperse "(listof string?)" xs))
  (unless (andmap string? xs)
    (raise-argument-error 'string-intersperse "(listof string?)" xs))
  (string-join xs sep))

;; string-quote : string? [#:quote-char char?] -> string?
;;   Quote s and escape embedded quote/backslash/control characters.
(define (string-quote s #:quote-char [quote-char #\"])
  (unless (string? s)
    (raise-argument-error 'string-quote "string?" s))
  (unless (char? quote-char)
    (raise-argument-error 'string-quote "char?" quote-char))
  (define (hex2 n)
    (define h (string-upcase (number->string n 16)))
    (if (= (string-length h) 1)
        (string-append "0" h)
        h))
  (define escaped
    (apply string-append
           (for/list ([ch (in-string s)])
             (cond
               [(char=? ch #\\) "\\\\"]
               [(char=? ch quote-char) (string-append "\\" (string quote-char))]
               [(char=? ch #\newline) "\\n"]
               [(char=? ch #\return) "\\r"]
               [(char=? ch #\tab) "\\t"]
               [(char=? ch #\backspace) "\\b"]
               [(char=? ch #\page) "\\f"]
               [else
                (define cp (char->integer ch))
                (if (or (< cp 32) (= cp 127))
                    (string-append "\\x" (hex2 cp))
                    (string ch))]))))
  (string-append (string quote-char) escaped (string quote-char)))

;; string-unquote : string? [#:quote-char char?] -> string?
;;   Remove surrounding quotes and unescape backslash escapes.
(define (string-unquote s #:quote-char [quote-char #\"])
  (unless (string? s)
    (raise-argument-error 'string-unquote "string?" s))
  (unless (char? quote-char)
    (raise-argument-error 'string-unquote "char?" quote-char))
  (define n (string-length s))
  (unless (and (>= n 2)
               (char=? (string-ref s 0) quote-char)
               (char=? (string-ref s (sub1 n)) quote-char))
    (raise-arguments-error 'string-unquote
                           "expected matching outer quotes"
                           "input" s
                           "quote-char" quote-char))
  (define body (substring s 1 (sub1 n)))
  (define m (string-length body))
  (define (hex-digit? ch)
    (or (char<=? #\0 ch #\9)
        (char<=? #\a (char-downcase ch) #\f)))
  (define (parse-hex i j)
    (string->number (substring body i j) 16))
  (define out (open-output-string))
  (let loop ([i 0])
    (cond
      [(>= i m)
       (get-output-string out)]
      [else
       (define ch (string-ref body i))
       (cond
         [(not (char=? ch #\\))
          (write-char ch out)
          (loop (add1 i))]
         [(= (add1 i) m)
          (raise-arguments-error 'string-unquote
                                 "trailing backslash escape"
                                 "input" s)]
         [else
          (define esc (string-ref body (add1 i)))
          (cond
            [(char=? esc #\\) (write-char #\\ out) (loop (+ i 2))]
            [(char=? esc quote-char) (write-char quote-char out) (loop (+ i 2))]
            [(char=? esc #\n) (write-char #\newline out) (loop (+ i 2))]
            [(char=? esc #\r) (write-char #\return out) (loop (+ i 2))]
            [(char=? esc #\t) (write-char #\tab out) (loop (+ i 2))]
            [(char=? esc #\b) (write-char #\backspace out) (loop (+ i 2))]
            [(char=? esc #\f) (write-char #\page out) (loop (+ i 2))]
            [(char=? esc #\x)
             (define h0 (+ i 2))
             (cond
               [(>= h0 m)
                (raise-arguments-error 'string-unquote
                                       "incomplete hex escape"
                                       "input" s
                                       "index" i)]
               [else
                (define hch (string-ref body h0))
                (cond
                  [(and (hex-digit? hch))
                   (define end
                     (let find-end ([j h0])
                       (cond
                         [(>= j m) m]
                         [(hex-digit? (string-ref body j)) (find-end (add1 j))]
                         [else j])))
                   (if (and (< end m) (char=? (string-ref body end) #\;))
                       (let ([cp (parse-hex h0 end)])
                         (write-char (integer->char cp) out)
                         (loop (add1 end)))
                       (let ([h1 (add1 h0)])
                         (if (and (< h1 m)
                                  (hex-digit? (string-ref body h0))
                                  (hex-digit? (string-ref body h1)))
                             (let ([cp (parse-hex h0 (+ h1 1))])
                               (write-char (integer->char cp) out)
                               (loop (+ i 4)))
                             (raise-arguments-error 'string-unquote
                                                    "hex escape must be \\xNN or \\x...;"
                                                    "input" s
                                                    "index" i))))]
                  [else
                   (raise-arguments-error 'string-unquote
                                          "hex escape must be followed by hexadecimal digits"
                                          "input" s
                                          "index" i)])])]
            [else
             (raise-arguments-error 'string-unquote
                                    "unknown escape sequence"
                                    "input" s
                                    "index" i)])])])))

;; string-escape-regexp : string? -> string?
;;   Escape s so it can be used as a literal regular-expression pattern.
(define (string-escape-regexp s)
  (unless (string? s)
    (raise-argument-error 'string-escape-regexp "string?" s))
  (regexp-quote s))

;; string-escape-visible : string? -> string?
;;   Escape control characters and backslash into visible sequences.
(define (string-escape-visible s)
  (unless (string? s)
    (raise-argument-error 'string-escape-visible "string?" s))
  (define (hex2 n)
    (define h (string-upcase (number->string n 16)))
    (if (= (string-length h) 1)
        (string-append "0" h)
        h))
  (define parts
    (for/list ([ch (in-string s)])
      (cond
        [(char=? ch #\\) "\\\\"]
        [(char=? ch #\newline) "\\n"]
        [(char=? ch #\return) "\\r"]
        [(char=? ch #\tab) "\\t"]
        [(char=? ch #\backspace) "\\b"]
        [(char=? ch #\page) "\\f"]
        [else
         (define cp (char->integer ch))
         (if (or (< cp 32) (= cp 127))
             (string-append "\\x" (hex2 cp))
             (string ch))])))
  (apply string-append parts))

;; string-unescape-visible : string? -> string?
;;   Parse visible escapes such as \n, \t, \\, and \xNN or \x...;.
(define (string-unescape-visible s)
  (unless (string? s)
    (raise-argument-error 'string-unescape-visible "string?" s))
  (define n (string-length s))
  (define (hex-digit? ch)
    (or (char<=? #\0 ch #\9)
        (char<=? #\a (char-downcase ch) #\f)))
  (define (parse-hex i j)
    (string->number (substring s i j) 16))
  (define out (open-output-string))
  (let loop ([i 0])
    (cond
      [(>= i n)
       (get-output-string out)]
      [else
       (define ch (string-ref s i))
       (cond
         [(not (char=? ch #\\))
          (write-char ch out)
          (loop (add1 i))]
         [(= (add1 i) n)
          (raise-arguments-error 'string-unescape-visible
                                 "trailing backslash escape"
                                 "input" s)]
         [else
          (define esc (string-ref s (add1 i)))
          (cond
            [(char=? esc #\\) (write-char #\\ out) (loop (+ i 2))]
            [(char=? esc #\n) (write-char #\newline out) (loop (+ i 2))]
            [(char=? esc #\r) (write-char #\return out) (loop (+ i 2))]
            [(char=? esc #\t) (write-char #\tab out) (loop (+ i 2))]
            [(char=? esc #\b) (write-char #\backspace out) (loop (+ i 2))]
            [(char=? esc #\f) (write-char #\page out) (loop (+ i 2))]
            [(char=? esc #\x)
             (define h0 (+ i 2))
             (cond
               [(>= h0 n)
                (raise-arguments-error 'string-unescape-visible
                                       "incomplete hex escape"
                                       "input" s
                                       "index" i)]
               [else
                (define hch (string-ref s h0))
                (cond
                  ;; \x...; form
                  [(and (hex-digit? hch))
                   (define end
                     (let find-end ([j h0])
                       (cond
                         [(>= j n) n]
                         [(hex-digit? (string-ref s j)) (find-end (add1 j))]
                         [else j])))
                   (if (and (< end n) (char=? (string-ref s end) #\;))
                       (let ([cp (parse-hex h0 end)])
                         (unless (and cp (char? (integer->char cp)))
                           (raise-arguments-error 'string-unescape-visible
                                                  "invalid Unicode scalar value"
                                                  "input" s
                                                  "index" i))
                         (write-char (integer->char cp) out)
                         (loop (add1 end)))
                       (let ([h1 (add1 h0)])
                         (if (and (< h1 n)
                                  (hex-digit? (string-ref s h0))
                                  (hex-digit? (string-ref s h1)))
                             (let ([cp (parse-hex h0 (+ h1 1))])
                               (write-char (integer->char cp) out)
                               (loop (+ i 4)))
                             (raise-arguments-error 'string-unescape-visible
                                                    "hex escape must be \\xNN or \\x...;"
                                                    "input" s
                                                    "index" i))))]
                  [else
                   (raise-arguments-error 'string-unescape-visible
                                          "hex escape must be followed by hexadecimal digits"
                                          "input" s
                                          "index" i)])])]
            [else
             (raise-arguments-error 'string-unescape-visible
                                    "unknown escape sequence"
                                    "input" s
                                    "index" i)])])])))

;; string-escape-json : string? -> string?
;;   Escape a string for use as JSON string content (without outer quotes).
(define (string-escape-json s)
  (unless (string? s)
    (raise-argument-error 'string-escape-json "string?" s))
  (define (hex4 n)
    (define h (string-upcase (number->string n 16)))
    (string-append (make-string (max 0 (- 4 (string-length h))) #\0) h))
  (apply string-append
         (for/list ([ch (in-string s)])
           (define cp (char->integer ch))
           (cond
             [(char=? ch #\") "\\\""]
             [(char=? ch #\\) "\\\\"]
             [(char=? ch #\/) "\\/"]
             [(char=? ch #\backspace) "\\b"]
             [(char=? ch #\page) "\\f"]
             [(char=? ch #\newline) "\\n"]
             [(char=? ch #\return) "\\r"]
             [(char=? ch #\tab) "\\t"]
             [(< cp 32) (string-append "\\u" (hex4 cp))]
             [else (string ch)]))))

;; string-unescape-json : string? -> string?
;;   Unescape JSON string content (without outer quotes).
(define (string-unescape-json s)
  (unless (string? s)
    (raise-argument-error 'string-unescape-json "string?" s))
  (define n (string-length s))
  (define (hex-digit? ch)
    (or (char<=? #\0 ch #\9)
        (char<=? #\a (char-downcase ch) #\f)))
  (define (hex-value i j)
    (string->number (substring s i j) 16))
  (define (parse-u4 i)
    (define j (+ i 4))
    (unless (<= j n)
      (raise-arguments-error 'string-unescape-json
                             "incomplete \\u escape"
                             "input" s
                             "index" (- i 2)))
    (for ([k (in-range i j)])
      (unless (hex-digit? (string-ref s k))
        (raise-arguments-error 'string-unescape-json
                               "invalid hex digit in \\u escape"
                               "input" s
                               "index" k)))
    (hex-value i j))
  (define (high-surrogate? cp) (<= #xD800 cp #xDBFF))
  (define (low-surrogate? cp) (<= #xDC00 cp #xDFFF))
  (define out (open-output-string))
  (let loop ([i 0])
    (cond
      [(>= i n)
       (get-output-string out)]
      [else
       (define ch (string-ref s i))
       (cond
         [(not (char=? ch #\\))
          (write-char ch out)
          (loop (add1 i))]
         [(= (add1 i) n)
          (raise-arguments-error 'string-unescape-json
                                 "trailing backslash escape"
                                 "input" s)]
         [else
          (define esc (string-ref s (add1 i)))
          (cond
            [(char=? esc #\") (write-char #\" out) (loop (+ i 2))]
            [(char=? esc #\\) (write-char #\\ out) (loop (+ i 2))]
            [(char=? esc #\/) (write-char #\/ out) (loop (+ i 2))]
            [(char=? esc #\b) (write-char #\backspace out) (loop (+ i 2))]
            [(char=? esc #\f) (write-char #\page out) (loop (+ i 2))]
            [(char=? esc #\n) (write-char #\newline out) (loop (+ i 2))]
            [(char=? esc #\r) (write-char #\return out) (loop (+ i 2))]
            [(char=? esc #\t) (write-char #\tab out) (loop (+ i 2))]
            [(char=? esc #\u)
             (define cp1 (parse-u4 (+ i 2)))
             (cond
               [(high-surrogate? cp1)
                (unless (and (<= (+ i 12) n)
                             (char=? (string-ref s (+ i 6)) #\\)
                             (char=? (string-ref s (+ i 7)) #\u))
                  (raise-arguments-error 'string-unescape-json
                                         "high surrogate not followed by low surrogate"
                                         "input" s
                                         "index" i))
                (define cp2 (parse-u4 (+ i 8)))
                (unless (low-surrogate? cp2)
                  (raise-arguments-error 'string-unescape-json
                                         "invalid low surrogate"
                                         "input" s
                                         "index" (+ i 8)))
                (define scalar
                  (+ #x10000
                     (* (- cp1 #xD800) #x400)
                     (- cp2 #xDC00)))
                (write-char (integer->char scalar) out)
                (loop (+ i 12))]
               [(low-surrogate? cp1)
                (raise-arguments-error 'string-unescape-json
                                       "unexpected low surrogate"
                                       "input" s
                                       "index" i)]
               [else
                (write-char (integer->char cp1) out)
                (loop (+ i 6))])]
            [else
             (raise-arguments-error 'string-unescape-json
                                    "unknown escape sequence"
                                    "input" s
                                    "index" i)])])])))

;; string-strip-ansi : string? -> string?
;;   Remove ANSI/VT control sequences such as CSI and OSC escapes.
(define (string-strip-ansi s)
  (unless (string? s)
    (raise-argument-error 'string-strip-ansi "string?" s))
  (define no-csi
    (regexp-replace* #px"\u001b\\[[0-?]*[ -/]*[@-~]" s ""))
  (define no-osc
    (regexp-replace* #px"\u001b\\][^\u0007\u001b]*(?:\u0007|\u001b\\\\)" no-csi ""))
  (regexp-replace* #px"\u009b[0-?]*[ -/]*[@-~]" no-osc ""))

;; string-squeeze : string? [(or/c char? char-set? string? (-> char? any/c))] -> string?
;;   Collapse consecutive matching characters in s to a single character.
(define (string-squeeze s [to-squeeze char-whitespace?])
  (unless (string? s)
    (raise-argument-error 'string-squeeze "string?" s))
  (define raw-needle to-squeeze)
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))
  (define pred
    (cond
      [(char? needle)      (λ (ch) (char=? ch needle))]
      [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-squeeze
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))
  (define out (open-output-string))
  (let loop ([i 0] [n (string-length s)] [in-run? #f])
    (cond
      [(>= i n)
       (get-output-string out)]
      [else
       (define ch (string-ref s i))
       (define match? (pred ch))
       (cond
         [(and match? in-run?)
          (loop (add1 i) n #t)]
         [else
          (write-char ch out)
          (loop (add1 i) n match?)])])))

;; make-char-matcher : symbol? any/c -> (-> char? any/c)
;;   Build a character predicate from char, char-set, string (as char-set), or unary predicate.
(define (make-char-matcher who raw)
  (define needle
    (if (string? raw)
        (string->char-set raw)
        raw))
  (cond
    [(char? needle)      (λ (ch) (char=? ch needle))]
    [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
    [(and (procedure? needle)
          (procedure-arity-includes? needle 1))
     needle]
    [else
     (raise-argument-error
      who
      "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
      needle)]))

;; parse-delimited : symbol? string? (-> char? any/c) exact-integer? exact-integer?
;;                  (or/c #f char?) (or/c #f char?) boolean?
;;                  -> (listof string?)
;;   Parse delimited text with optional quote/escape handling.
(define (parse-delimited who s sep? start* end* quote-char escape-char keep-empty?)
  (define (valid-esc? i)
    (and escape-char (char=? (string-ref s i) escape-char)))
  (define (valid-quote? i)
    (and quote-char (char=? (string-ref s i) quote-char)))
  (let loop ([i start*]
             [in-quote? #f]
             [out (open-output-string)]
             [acc (list)])
    (cond
      [(>= i end*)
       (when in-quote?
         (raise-arguments-error who
                                "unterminated quoted field"
                                "input" s
                                "start" start*
                                "end" end*))
       (define field (get-output-string out))
       (if (or keep-empty? (not (string=? field "")))
           (reverse (cons field acc))
           (reverse acc))]
      [else
       (define ch (string-ref s i))
       (cond
         [(valid-esc? i)
          (cond
            [(< (add1 i) end*)
             (write-char (string-ref s (add1 i)) out)
             (loop (+ i 2) in-quote? out acc)]
            [else
             (write-char ch out)
             (loop (add1 i) in-quote? out acc)])]
         [(valid-quote? i)
          (loop (add1 i) (not in-quote?) out acc)]
         [(and (not in-quote?) (sep? ch))
          (define field (get-output-string out))
          (cond
            [(or keep-empty? (not (string=? field "")))
             (loop (add1 i)
                   #f
                   (open-output-string)
                   (cons field acc))]
            [else
             (loop (add1 i)
                   #f
                   (open-output-string)
                   acc)])]
         [else
          (write-char ch out)
          (loop (add1 i) in-quote? out acc)])])))

;; string-tokenize : string?
;;                  [(or/c char? char-set? string? (-> char? any/c))]
;;                  [exact-integer?] [exact-integer?]
;;                  [#:quote (or/c #f char?)] [#:escape (or/c #f char?)]
;;                  -> (listof string?)
;;   Split s into non-empty tokens using separator matcher, with optional quoting and escaping.
(define (string-tokenize s
                         [to-separate char-whitespace?]
                         [start 0]
                         [end (string-length s)]
                         #:quote [quote-char #f]
                         #:escape [escape-char #\\])
  (unless (string? s)
    (raise-argument-error 'string-tokenize "string?" s))
  (unless (or (not quote-char) (char? quote-char))
    (raise-argument-error 'string-tokenize "(or/c #f char?)" quote-char))
  (unless (or (not escape-char) (char? escape-char))
    (raise-argument-error 'string-tokenize "(or/c #f char?)" escape-char))
  (define sep? (make-char-matcher 'string-tokenize to-separate))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-tokenize n start end))
  (parse-delimited 'string-tokenize s sep? start* end* quote-char escape-char #f))

;; string-fields : string?
;;                [(or/c char? char-set? string? (-> char? any/c))]
;;                [exact-integer?] [exact-integer?]
;;                [#:quote (or/c #f char?)] [#:escape (or/c #f char?)]
;;                [#:widths (or/c #f (listof exact-positive-integer?))]
;;                [#:include-rest? boolean?]
;;                -> (listof string?)
;;   Split s into fields; delimiter mode keeps empty fields, widths mode slices fixed column widths.
(define (string-fields s
                       [to-separate #\,]
                       [start 0]
                       [end (string-length s)]
                       #:quote [quote-char #f]
                       #:escape [escape-char #\\]
                       #:widths [widths #f]
                       #:include-rest? [include-rest? #f])
  (unless (string? s)
    (raise-argument-error 'string-fields "string?" s))
  (unless (or (not quote-char) (char? quote-char))
    (raise-argument-error 'string-fields "(or/c #f char?)" quote-char))
  (unless (or (not escape-char) (char? escape-char))
    (raise-argument-error 'string-fields "(or/c #f char?)" escape-char))
  (unless (boolean? include-rest?)
    (raise-argument-error 'string-fields "boolean?" include-rest?))
  (unless (or (not widths)
              (and (list? widths)
                   (andmap exact-positive-integer? widths)))
    (raise-argument-error
     'string-fields
     "(or/c #f (listof exact-positive-integer?))"
     widths))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-fields n start end))
  (if widths
      (let ([fields
             (let loop ([i start*] [ws widths] [acc (list)])
               (cond
                 [(null? ws) (reverse acc)]
                 [else
                  (define w  (car ws))
                  (define j  (min end* (+ i w)))
                  (define f  (if (<= i end*) (substring s i j) ""))
                  (define i2 (min end* (+ i w)))
                  (loop i2 (cdr ws) (cons f acc))]))])
        (if include-rest?
            (append fields
                    (list (if (<= start* end*)
                              (substring s (min end* (+ start* (apply + widths))) end*)
                              "")))
            fields))
      (let ([sep? (make-char-matcher 'string-fields to-separate)])
        (parse-delimited 'string-fields s sep? start* end* quote-char escape-char #t))))

;; string-scan : string?
;;               (or/c string? char? char-set? (-> char? any/c))
;;               [exact-integer?] [exact-integer?]
;;               [#:overlap? boolean?]
;;               -> (-> (or/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) #f))
;;   Return a generator that yields successive (cons start end) match ranges.
(define (string-scan s matcher [start 0] [end (string-length s)] #:overlap? [overlap? #f])
  (unless (string? s)
    (raise-argument-error 'string-scan "string?" s))
  (unless (boolean? overlap?)
    (raise-argument-error 'string-scan "boolean?" overlap?))
  (unless (or (string? matcher)
              (char? matcher)
              (char-set? matcher)
              (and (procedure? matcher)
                   (procedure-arity-includes? matcher 1)))
    (raise-argument-error
     'string-scan
     "(or/c string? char? char-set? (procedure-arity-includes/c 1))"
     matcher))
  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-scan n start end))
  (cond
    [(string? matcher)
     (define needle matcher)
     (define m      (string-length needle))
     (generator ()
       (cond
         ((zero? m)
          (let loop ([i start*])
            (when (<= i end*)
              (yield (cons i i))
              (loop (add1 i)))))
         (else
          (let loop ([i start*])
            (define j (string-find-needle s needle i end*))
            (when j
              (yield (cons j (+ j m)))
              (loop (+ j (if overlap? 1 m)))))))
       #f)]
    [else
     (define pred (make-char-matcher 'string-scan matcher))
     (generator ()
       (let loop ([i start*])
         (when (< i end*)
           (when (pred (string-ref s i))
             (yield (cons i (add1 i))))
           (loop (add1 i))))
       #f)]))

;; string-remove-prefix : string? string? -> string?
;;   Remove prefix from s when present; otherwise return s unchanged.
(define (string-remove-prefix s prefix)
  (unless (string? s)
    (raise-argument-error 'string-remove-prefix "string?" s))
  (unless (string? prefix)
    (raise-argument-error 'string-remove-prefix "string?" prefix))
  (define n (string-length s))
  (define m (string-length prefix))
  (if (and (<= m n)
           (string=? (substring s 0 m) prefix))
      (substring s m n)
      s))

;; string-remove-suffix : string? string? -> string?
;;   Remove suffix from s when present; otherwise return s unchanged.
(define (string-remove-suffix s suffix)
  (unless (string? s)
    (raise-argument-error 'string-remove-suffix "string?" s))
  (unless (string? suffix)
    (raise-argument-error 'string-remove-suffix "string?" suffix))
  (define n (string-length s))
  (define m (string-length suffix))
  (if (and (<= m n)
           (string=? (substring s (- n m) n) suffix))
      (substring s 0 (- n m))
      s))

;; string-ensure-prefix : string? string? -> string?
;;   Ensure s starts with prefix by adding it when missing.
(define (string-ensure-prefix s prefix)
  (unless (string? s)
    (raise-argument-error 'string-ensure-prefix "string?" s))
  (unless (string? prefix)
    (raise-argument-error 'string-ensure-prefix "string?" prefix))
  (define n (string-length s))
  (define m (string-length prefix))
  (if (and (<= m n)
           (string=? (substring s 0 m) prefix))
      s
      (string-append prefix s)))

;; string-ensure-suffix : string? string? -> string?
;;   Ensure s ends with suffix by adding it when missing.
(define (string-ensure-suffix s suffix)
  (unless (string? s)
    (raise-argument-error 'string-ensure-suffix "string?" s))
  (unless (string? suffix)
    (raise-argument-error 'string-ensure-suffix "string?" suffix))
  (define n (string-length s))
  (define m (string-length suffix))
  (if (and (<= m n)
           (string=? (substring s (- n m) n) suffix))
      s
      (string-append s suffix)))

;; string-count : string? (or/c char? char-set? string? (-> char? any/c))
;;                [exact-integer?] [exact-integer?]
;;                -> exact-nonnegative-integer?
;;   Count characters in s matching a char, char-set, or predicate.
(define (string-count s to-count [start 0] [end (string-length s)])
  (define raw-needle to-count)

  (unless (string? s)
    (raise-argument-error 'string-count "string?" s))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))

  (define pred
    (cond
      [(char? needle)
       (λ (ch) (char=? ch needle))]
      [(char-set? needle)
       (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-count
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-count n start end))

  (for/sum ([i (in-range start* end*)])
    (if (pred (string-ref s i)) 1 0)))

;; string-index : string? (or/c char? char-set? string? (-> char? any/c))
;;                [exact-integer?] [exact-integer?]
;;                -> (or/c exact-nonnegative-integer? #f)
;;   Find the first index in [start, end) matching char/char-set/pred.
(define (string-index s char/char-set/pred [start 0] [end (string-length s)])
  (define raw-needle char/char-set/pred)

  (unless (string? s)
    (raise-argument-error 'string-index "string?" s))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))

  (define pred
    (cond
      [(char? needle)      (λ (ch) (char=? ch needle))]
      [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-index
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-index n start end))

  (let loop ([i start*])
    (cond
      [(>= i end*)                   #f]
      [(pred (string-ref s i))       i]
      [else                          (loop (add1 i))])))

;; string-index-right : string? (or/c char? char-set? string? (-> char? any/c))
;;                      [exact-integer?] [exact-integer?]
;;                      -> (or/c exact-nonnegative-integer? #f)
;;   Find the rightmost index in [start, end) matching char/char-set/pred.
(define (string-index-right s char/char-set/pred [start 0] [end (string-length s)])
  (define raw-needle char/char-set/pred)

  (unless (string? s)
    (raise-argument-error 'string-index-right "string?" s))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))

  (define pred
    (cond
      [(char? needle)      (λ (ch) (char=? ch needle))]
      [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-index-right
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-index-right n start end))

  (let loop ([i (sub1 end*)])
    (cond
      [(< i start*)                  #f]
      [(pred (string-ref s i))       i]
      [else                          (loop (sub1 i))])))

;; string-skip : string? (or/c char? char-set? string? (-> char? any/c))
;;               [exact-integer?] [exact-integer?]
;;               -> (or/c exact-nonnegative-integer? #f)
;;   Find the first index in [start, end) that does not match char/char-set/pred.
(define (string-skip s to-skip [start 0] [end (string-length s)])
  (define raw-needle to-skip)

  (unless (string? s)
    (raise-argument-error 'string-skip "string?" s))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))

  (define pred
    (cond
      [(char? needle)      (λ (ch) (char=? ch needle))]
      [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-skip
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-skip n start end))

  (let loop ([i start*])
    (cond
      [(>= i end*)                     #f]
      [(not (pred (string-ref s i)))   i]
      [else                            (loop (add1 i))])))

;; string-skip-right : string? (or/c char? char-set? string? (-> char? any/c))
;;                     [exact-integer?] [exact-integer?]
;;                     -> (or/c exact-nonnegative-integer? #f)
;;   Find the rightmost index in [start, end) that does not match char/char-set/pred.
(define (string-skip-right s to-skip [start 0] [end (string-length s)])
  (define raw-needle to-skip)

  (unless (string? s)
    (raise-argument-error 'string-skip-right "string?" s))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))

  (define pred
    (cond
      [(char? needle)      (λ (ch) (char=? ch needle))]
      [(char-set? needle)  (λ (ch) (char-set-member? needle ch))]
      [(and (procedure? needle)
            (procedure-arity-includes? needle 1))
       needle]
      [else
       (raise-argument-error
        'string-skip-right
        "(or/c char? char-set? string? (procedure-arity-includes/c 1))"
        needle)]))

  (define n (string-length s))
  (define-values (start* end*) (normalize-start/end 'string-skip-right n start end))

  (let loop ([i (sub1 end*)])
    (cond
      [(< i start*)                    #f]
      [(not (pred (string-ref s i)))   i]
      [else                            (loop (sub1 i))])))

;; string-trim-left : string?
;;                    [(or/c char? char-set? string? (-> char? any/c))]
;;                    [exact-integer?] [exact-integer?]
;;                    -> string?
;;   Trim matching characters from the left side of the selected substring.
(define (string-trim-left s . args)
  (define n (string-length s))
  (define-values (raw-needle start end)
    (case (length args)
      [(0) (values char-whitespace? 0 n)]
      [(1) (values (list-ref args 0) 0 n)]
      [(2) (values (list-ref args 0) (list-ref args 1) n)]
      [(3) (values (list-ref args 0) (list-ref args 1) (list-ref args 2))]
      [else
       (raise-arity-error 'string-trim-left "(string? [matcher] [start] [end])"
                          (cons s args))]))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))
  (define-values (start* end*) (normalize-start/end 'string-trim-left n start end))

  (define i (string-skip s needle start* end*))
  (if i
      (substring s i end*)
      ""))

;; string-trim-right : string?
;;                     [(or/c char? char-set? string? (-> char? any/c))]
;;                     [exact-integer?] [exact-integer?]
;;                     -> string?
;;   Trim matching characters from the right side of the selected substring.
(define (string-trim-right s . args)
  (define n (string-length s))
  (define-values (raw-needle start end)
    (case (length args)
      [(0) (values char-whitespace? 0 n)]
      [(1) (values (list-ref args 0) 0 n)]
      [(2) (values (list-ref args 0) (list-ref args 1) n)]
      [(3) (values (list-ref args 0) (list-ref args 1) (list-ref args 2))]
      [else
       (raise-arity-error 'string-trim-right "(string? [matcher] [start] [end])"
                          (cons s args))]))
  (define needle
    (if (string? raw-needle)
        (string->char-set raw-needle)
        raw-needle))
  (define-values (start* end*) (normalize-start/end 'string-trim-right n start end))

  (define i (string-skip-right s needle start* end*))
  (if i
      (substring s start* (add1 i))
      ""))

(module+ test
  (require rackunit
           "char-set.rkt")

  (check-equal? (string-split-at "")        '(""))
  (check-equal? (string-split-at "abc")     '("abc"))
  (check-equal? (string-split-at "abc" 0)   '("" "abc"))
  (check-equal? (string-split-at "abc" 3)   '("abc" ""))
  (check-equal? (string-split-at "abc" -1)  '("ab" "c"))
  (check-equal? (string-split-at "abc" -10) '("" "abc"))
  (check-equal? (string-split-at "abc" 10)  '("abc" ""))
  (check-equal? (string-split-at "abcdef" 2 4)
                '("ab" "cd" "ef"))

  ;; Fast paths
  (check-equal? (string-split-at "abc" 1)
                '("a" "bc"))

  ;; Unsorted indices are fine (we sort).
  (check-equal? (string-split-at "abcdef" 4 2)
                '("ab" "cd" "ef"))

  ;; Duplicate indices are ignored (we deduplicate).
  (check-equal? (string-split-at "abc" 1 1 2 2)
                '("a" "b" "c"))

  ;; Including 0 and n explicitly shouldn't change anything.
  (check-equal? (string-split-at "abc" 0 1 3)
                '("" "a" "bc" ""))
  (check-equal? (string-split-at "abcdef" -1 -4)
                '("ab" "cde" "f"))

  ;; Errors
  (check-exn exn:fail:contract?
             (λ () (string-split-at "abc" 1.5)))

  (check-equal? (string-count-needle "banana" "na") 2)
  (check-equal? (string-count-needle "aaaa" "aa") 2)   ; non-overlapping
  (check-equal? (string-count-needle "aaaa" "aaa") 1)
  (check-equal? (string-count-needle "abc" "d") 0)

  ;; start/end like Python slicing bounds (end exclusive)
  (check-equal? (string-count-needle "banana" "na" 0 6) 2)
  (check-equal? (string-count-needle "banana" "na" 3 6) 1)
  (check-equal? (string-count-needle "banana" "na" 4 6) 1)
  (check-equal? (string-count-needle "banana" "na" 5 6) 0)
  (check-equal? (string-count-needle "banana" "na" -4 -1) 1)

  ;; empty needle
  (check-equal? (string-count-needle "abc" "") 4)
  (check-equal? (string-count-needle "abc" "" 1 3) 3)

  ;; errors
  (check-equal? (string-count-needle "abc" "a" -1) 0)
  (check-equal? (string-count-needle "abc" "a" 2 5) 0)
  (check-equal? (string-count-needle "abc" "a" 3 2) 0)

  ;; string-find-needle
  (check-equal? (string-find-needle "banana" "na") 2)
  (check-equal? (string-find-needle "banana" "na" 3 6) 4)
  (check-equal? (string-find-needle "banana" "na" -4 -1) 2)
  (check-false  (string-find-needle "banana" "xy"))
  (check-equal? (string-find-needle "abc" "") 0)
  (check-equal? (string-find-needle "abc" "" 1 3) 1)

  ;; string-find-last-needle
  (check-equal? (string-find-last-needle "banana" "na") 4)
  (check-equal? (string-find-last-needle "banana" "na" 0 5) 2)
  (check-equal? (string-find-last-needle "banana" "na" -4 -1) 2)
  (check-false  (string-find-last-needle "banana" "xy"))
  (check-equal? (string-find-last-needle "abc" "") 3)
  (check-equal? (string-find-last-needle "abc" "" 1 3) 3)

  ;; find-needle errors
  (check-exn exn:fail:contract? (λ () (string-find-needle 123 "a")))
  (check-exn exn:fail:contract? (λ () (string-find-needle "abc" 1)))
  (check-false  (string-find-needle "abc" "a" -1 2))
  (check-false  (string-find-last-needle "abc" "a" 1 4))
  (check-false  (string-find-last-needle "abc" "a" 2 1))

  ;; string-find-all-needle
  (check-equal? (string-find-all-needle "banana" "na")
                '(2 4))
  (check-equal? (string-find-all-needle "banana" "na" #:ranges? #t)
                (list (cons 2 4) (cons 4 6)))
  (check-equal? (string-find-all-needle "aaaa" "aa")
                '(0 2))
  (check-equal? (string-find-all-needle "aaaa" "aa" #:overlap? #t)
                '(0 1 2))
  (check-equal? (string-find-all-needle "abc" "" 1 3)
                '(1 2 3))
  (check-equal? (string-find-all-needle "banana" "na" -4 -1)
                '(2))
  (check-exn exn:fail:contract? (λ () (string-find-all-needle 123 "a")))
  (check-exn exn:fail:contract? (λ () (string-find-all-needle "abc" 1)))
  (check-exn exn:fail:contract? (λ () (string-find-all-needle "abc" "a" #:ranges? 'x)))

  ;; string-partition
  (check-equal? (call-with-values (λ () (string-partition "a:b:c" ":")) list)
                '("a" ":" "b:c"))
  (check-equal? (call-with-values (λ () (string-partition "abc" ":")) list)
                '("abc" "" ""))
  (check-equal? (call-with-values (λ () (string-partition "banana" "na")) list)
                '("ba" "na" "na"))
  (check-equal? (call-with-values (λ () (string-partition "banana" "na" 2 6)) list)
                '("" "na" "na"))
  (check-equal? (call-with-values (λ () (string-partition "banana" "na" -4 -1)) list)
                '("" "na" "n"))
  (check-equal? (call-with-values (λ () (string-partition "abc" "")) list)
                '("" "" "abc"))
  (check-exn exn:fail:contract? (λ () (string-partition 123 ":")))
  (check-exn exn:fail:contract? (λ () (string-partition "abc" 1)))
  (check-equal? (call-with-values (λ () (string-partition "abc" ":" -1 2)) list)
                '("" "" ""))

  ;; string-partition-right
  (check-equal? (call-with-values (λ () (string-partition-right "a:b:c" ":")) list)
                '("a:b" ":" "c"))
  (check-equal? (call-with-values (λ () (string-partition-right "abc" ":")) list)
                '("abc" "" ""))
  (check-equal? (call-with-values (λ () (string-partition-right "banana" "na")) list)
                '("bana" "na" ""))
  (check-equal? (call-with-values (λ () (string-partition-right "banana" "na" 0 5)) list)
                '("ba" "na" "n"))
  (check-equal? (call-with-values (λ () (string-partition-right "banana" "na" -4 -1)) list)
                '("" "na" "n"))
  (check-equal? (call-with-values (λ () (string-partition-right "abc" "")) list)
                '("abc" "" ""))
  (check-exn exn:fail:contract? (λ () (string-partition-right 123 ":")))
  (check-exn exn:fail:contract? (λ () (string-partition-right "abc" 1)))
  (check-equal? (call-with-values (λ () (string-partition-right "abc" ":" -1 2)) list)
                '("" "" ""))

  ;; string-replace-range
  (check-equal? (string-replace-range "abcdef" 2 4 "XY") "abXYef")
  (check-equal? (string-replace-range "abcdef" -4 -2 "XY") "abXYef")
  (check-equal? (string-replace-range "abcdef" 4 2 "XY") "abXYef")
  (check-equal? (string-replace-range "abcdef" 10 12 "XY") "abcdefXY")
  (check-equal? (string-replace-range "abcdef" -20 -10 "XY") "XYabcdef")
  (check-exn exn:fail:contract? (λ () (string-replace-range 123 0 1 "x")))
  (check-exn exn:fail:contract? (λ () (string-replace-range "abc" 0 1 123)))

  ;; string-repeat
  (check-equal? (string-repeat "ab" 0) "")
  (check-equal? (string-repeat "ab" 3) "ababab")
  (check-exn exn:fail:contract? (λ () (string-repeat 123 2)))
  (check-exn exn:fail:contract? (λ () (string-repeat "ab" -1)))

  ;; string-reverse
  (check-equal? (string-reverse "") "")
  (check-equal? (string-reverse "abc") "cba")
  (check-exn exn:fail:contract? (λ () (string-reverse 123)))

  ;; string-wrap
  (check-equal? (string-wrap "" 5) "")
  (check-equal? (string-wrap "alpha beta gamma" 10) "alpha\nbeta gamma")
  (check-equal? (string-wrap "supercalifragilistic" 8) "supercalifragilistic")
  (check-equal? (string-wrap "supercalifragilistic" 8 #:preserve-words? #f)
                "supercal\nifragili\nstic")
  (check-equal? (string-wrap "abcdefghij" 4 #:mode 'hard)
                "abcd\nefgh\nij")
  (check-exn exn:fail:contract? (λ () (string-wrap 123 10)))
  (check-exn exn:fail:contract? (λ () (string-wrap "abc" 0)))
  (check-exn exn:fail:contract? (λ () (string-wrap "abc" 2 #:mode 'x)))

  ;; string-indent
  (check-equal? (string-indent "" 2) "  ")
  (check-equal? (string-indent "a\nb" 2) "  a\n  b")
  (check-equal? (string-indent "a\nb" "-> ") "-> a\n-> b")
  (check-equal? (string-indent "a\n" 1) " a\n ")
  (check-exn exn:fail:contract? (λ () (string-indent 123 2)))
  (check-exn exn:fail:contract? (λ () (string-indent "abc" -1)))

  ;; string-dedent
  (check-equal? (string-dedent "  a\n  b") "a\nb")
  (check-equal? (string-dedent "    a\n      b") "a\n  b")
  (check-equal? (string-dedent "\t\ta\n\t\tb") "a\nb")
  (check-equal? (string-dedent "  a\n\n  b") "a\n\nb")
  (check-equal? (string-dedent "") "")
  (check-exn exn:fail:contract? (λ () (string-dedent 123)))

  ;; string-elide
  (check-equal? (string-elide "abcdef" 10) "abcdef")
  (check-equal? (string-elide "abcdef" 5) "ab...")
  (check-equal? (string-elide "abcdef" 5 #:where 'left) "...ef")
  (check-equal? (string-elide "abcdef" 5 #:where 'middle) "a...f")
  (check-equal? (string-elide "abcdef" 5 #:ellipsis "..") "abc..")
  (check-equal? (string-elide "abcdef" 2) "..")
  (check-equal? (string-elide "abcdef" 0) "")
  (check-exn exn:fail:contract? (λ () (string-elide 123 3)))
  (check-exn exn:fail:contract? (λ () (string-elide "abc" -1)))
  (check-exn exn:fail:contract? (λ () (string-elide "abc" 2 #:where 'x)))

  ;; string-common-prefix
  (check-equal? (string-common-prefix "foobar" "foobaz") "fooba")
  (check-equal? (string-common-prefix "abc" "xyz") "")
  (check-equal? (string-common-prefix "abc" "abc") "abc")
  (check-exn exn:fail:contract? (λ () (string-common-prefix 1 "abc")))

  ;; string-common-suffix
  (check-equal? (string-common-suffix "foobar" "xxbar") "bar")
  (check-equal? (string-common-suffix "abc" "xyz") "")
  (check-equal? (string-common-suffix "abc" "abc") "abc")
  (check-exn exn:fail:contract? (λ () (string-common-suffix "abc" 1)))

  ;; string-levenshtein
  (check-equal? (string-levenshtein "" "") 0)
  (check-equal? (string-levenshtein "kitten" "sitting") 3)
  (check-equal? (string-levenshtein "flaw" "lawn") 2)
  (check-equal? (string-levenshtein "abc" "abc") 0)
  (check-exn exn:fail:contract? (λ () (string-levenshtein 1 "abc")))

  ;; string-jaro-winkler / string-similarity
  (check-equal? (string-jaro-winkler "" "") 1.0)
  (check-equal? (string-jaro-winkler "abc" "abc") 1.0)
  (check-true (<= 0.0 (string-jaro-winkler "martha" "marhta") 1.0))
  (check-true (> (string-jaro-winkler "martha" "marhta")
                 (string-jaro-winkler "martha" "xyz")))
  (check-equal? (string-similarity "dixon" "dicksonx")
                (string-jaro-winkler "dixon" "dicksonx"))
  (check-exn exn:fail:contract? (λ () (string-jaro-winkler "a" "b" #:prefix-scale 0.3)))

  ;; string-between
  (check-equal? (string-between "a[b]c" "[" "]") "b")
  (check-equal? (string-between "a[b]c[d]e" "[" "]" #:left-match 'last) "d")
  (check-equal? (string-between "a[b]c[d]e" "[" "]" #:right-match 'last) "b]c[d")
  (check-equal? (string-between "a[b]c" "[" "]" #:include-left? #t #:include-right? #t) "[b]")
  (check-equal? (string-between "a[b]c" #\[ #\]) "b")
  (check-equal? (string-between "x<ab>y" "<" ">" -5 -1) "ab")
  (check-false (string-between "abc" "[" "]"))
  (check-false (string-between "a[b" "[" "]"))
  (check-exn exn:fail:contract? (λ () (string-between 1 "[" "]")))
  (check-exn exn:fail:contract? (λ () (string-between "abc" "" "]")))
  (check-exn exn:fail:contract? (λ () (string-between "abc" "[" "]" #:left-match 'middle)))

  ;; string-slice
  (check-equal? (string-slice "abcdef") "abcdef")
  (check-equal? (string-slice "abcdef" 1 4) "bcd")
  (check-equal? (string-slice "abcdef" -3 -1) "de")
  (check-equal? (string-slice "abcdef" -100 100) "abcdef")
  (check-equal? (string-slice "abcdef" 4 2) "")
  (check-equal? (string-slice "abcdef" 99 100) "")
  (check-exn exn:fail:contract? (λ () (string-slice 123)))
  (check-exn exn:fail:contract? (λ () (string-slice "abc" 1.5)))
  (check-exn exn:fail:contract? (λ () (string-slice "abc" 0 1.5)))

  ;; string-slice/step
  (check-equal? (string-slice/step "abcdef") "abcdef")
  (check-equal? (string-slice/step "abcdef" 0 6 2) "ace")
  (check-equal? (string-slice/step "abcdef" 5 -1 -2) "")
  (check-equal? (string-slice/step "abcdef" 5 #f -2) "fdb")
  (check-equal? (string-slice/step "abcdef" #f #f -1) "fedcba")
  (check-equal? (string-slice/step "abcdef" -100 100 3) "ad")
  (check-equal? (string-slice/step "abcdef" 4 2 1) "")
  (check-exn exn:fail:contract? (λ () (string-slice/step "abc" 0 2 0)))
  (check-exn exn:fail:contract? (λ () (string-slice/step 123)))

  ;; string-at
  (check-equal? (string-at "abc" 0) #\a)
  (check-equal? (string-at "abc" -1) #\c)
  (check-equal? (string-at "abc" 3) #\c)
  (check-equal? (string-at "abc" 3 #\x) #\c)
  (check-equal? (string-at "abc" -10 #\x) #\a)
  (check-false  (string-at "" 0))
  (check-equal? (string-at "" 0 #\x) #\x)
  (check-exn exn:fail:contract? (λ () (string-at 123 0)))
  (check-exn exn:fail:contract? (λ () (string-at "abc" 1.5)))

  ;; string-lines
  (check-equal? (string-lines "") '())
  (check-equal? (string-lines "abc") '("abc"))
  (check-equal? (string-lines "a\nb") '("a" "b"))
  (check-equal? (string-lines "a\r\nb") '("a" "b"))
  (check-equal? (string-lines "a\rb") '("a" "b"))
  (check-equal? (string-lines "a\n") '("a"))
  (check-equal? (string-lines "\n") '(""))
  (check-equal? (string-lines "a\n\nb") '("a" "" "b"))
  (check-equal? (string-lines "a\r\r") '("a" ""))
  (check-exn exn:fail:contract? (λ () (string-lines 123)))

  ;; string-count-lines
  (check-equal? (string-count-lines "") 1)
  (check-equal? (string-count-lines "abc") 1)
  (check-equal? (string-count-lines "a\nb") 2)
  (check-equal? (string-count-lines "a\r\nb") 2)
  (check-equal? (string-count-lines "a\rb") 2)
  (check-equal? (string-count-lines "a\n") 2)
  (check-equal? (string-count-lines "\n") 2)
  (check-equal? (string-count-lines "\r\n") 2)
  (check-exn exn:fail:contract? (λ () (string-count-lines 123)))

  ;; string-line-start-indices
  (check-equal? (string-line-start-indices "") '(0))
  (check-equal? (string-line-start-indices "abc") '(0))
  (check-equal? (string-line-start-indices "a\nb") '(0 2))
  (check-equal? (string-line-start-indices "a\r\nb") '(0 3))
  (check-equal? (string-line-start-indices "a\rb") '(0 2))
  (check-equal? (string-line-start-indices "a\n") '(0 2))
  (check-equal? (string-line-start-indices "\n") '(0 1))
  (check-exn exn:fail:contract? (λ () (string-line-start-indices 123)))

  ;; string-normalize-newlines
  (check-equal? (string-normalize-newlines "") "")
  (check-equal? (string-normalize-newlines "a\nb") "a\nb")
  (check-equal? (string-normalize-newlines "a\r\nb") "a\nb")
  (check-equal? (string-normalize-newlines "a\rb") "a\nb")
  (check-equal? (string-normalize-newlines "\r\n\rx\r") "\n\nx\n")
  (check-exn exn:fail:contract? (λ () (string-normalize-newlines 123)))

  ;; string-expand-tabs
  (check-equal? (string-expand-tabs "") "")
  (check-equal? (string-expand-tabs "a\tb") "a       b")
  (check-equal? (string-expand-tabs "ab\tcd" #:tab-width 4) "ab  cd")
  (check-equal? (string-expand-tabs "\t" #:tab-width 4) "    ")
  (check-equal? (string-expand-tabs "\t" #:tab-width 4 #:start-column 2) "  ")
  (check-equal? (string-expand-tabs "a\n\tb" #:tab-width 4) "a\n    b")
  (check-equal? (string-expand-tabs "a\r\tb" #:tab-width 4) "a\r    b")
  (check-exn exn:fail:contract? (λ () (string-expand-tabs 123)))
  (check-exn exn:fail:contract? (λ () (string-expand-tabs "a\tb" #:tab-width 0)))

  ;; string-display-width
  (check-equal? (string-display-width "") 0)
  (check-equal? (string-display-width "abc") 3)
  (check-equal? (string-display-width "a\tb") 9)
  (check-equal? (string-display-width "\t" #:tab-width 4 #:start-column 2) 4)
  (check-equal? (string-display-width "a\nbc") 2)
  (check-equal? (string-display-width "ab\rc") 1)
  (check-equal? (string-display-width (string #\a #\nul #\b)) 2)
  (check-equal? (string-display-width "ab" #:start-column 5) 7)
  (check-exn exn:fail:contract? (λ () (string-display-width 123)))
  (check-exn exn:fail:contract? (λ () (string-display-width "abc" #:tab-width 0)))

  ;; string-chomp / string-chop-newline
  (check-equal? (string-chomp "") "")
  (check-equal? (string-chomp "abc") "abc")
  (check-equal? (string-chomp "abc\n") "abc")
  (check-equal? (string-chomp "abc\r\n") "abc")
  (check-equal? (string-chomp "abc\r") "abc\r")
  (check-equal? (string-chomp "abc\n\n") "abc\n")
  (check-equal? (string-chop-newline "abc\n") "abc")
  (check-equal? (string-chop-newline "abc\r\n") "abc")
  (check-exn exn:fail:contract? (λ () (string-chomp 123)))
  (check-exn exn:fail:contract? (λ () (string-chop-newline 123)))

  ;; string-capitalize
  (check-equal? (string-capitalize "") "")
  (check-equal? (string-capitalize "hello world") "Hello world")
  (check-equal? (string-capitalize "hELLO WORLD") "Hello world")
  (check-equal? (string-capitalize "123abc") "123abc")
  (check-exn exn:fail:contract? (λ () (string-capitalize 123)))

  ;; string-swapcase
  (check-equal? (string-swapcase "") "")
  (check-equal? (string-swapcase "AbC") "aBc")
  (check-equal? (string-swapcase "hello WORLD") "HELLO world")
  (check-equal? (string-swapcase "123!?") "123!?")
  (check-exn exn:fail:contract? (λ () (string-swapcase 123)))

  ;; string-rot13
  (check-equal? (string-rot13 "") "")
  (check-equal? (string-rot13 "abcXYZ") "nopKLM")
  (check-equal? (string-rot13 "Hello, World!") "Uryyb, Jbeyq!")
  (check-equal? (string-rot13 (string-rot13 "Racket")) "Racket")
  (check-exn exn:fail:contract? (λ () (string-rot13 123)))

  ;; string-pluralize
  (check-equal? (string-pluralize "cat") "cats")
  (check-equal? (string-pluralize "box") "boxes")
  (check-equal? (string-pluralize "church") "churches")
  (check-equal? (string-pluralize "city") "cities")
  (check-equal? (string-pluralize "boy") "boys")
  (check-equal? (string-pluralize "leaf") "leaves")
  (check-equal? (string-pluralize "knife") "knives")
  (check-exn exn:fail:contract? (λ () (string-pluralize 123)))

  ;; string-singularize
  (check-equal? (string-singularize "cats") "cat")
  (check-equal? (string-singularize "boxes") "box")
  (check-equal? (string-singularize "churches") "church")
  (check-equal? (string-singularize "cities") "city")
  (check-equal? (string-singularize "leaves") "leaf")
  (check-equal? (string-singularize "knives") "knif")
  (check-equal? (string-singularize "bus") "bu")
  (check-equal? (string-singularize "fish") "fish")
  (check-exn exn:fail:contract? (λ () (string-singularize 123)))

  ;; string-ensure-ends-with-newline
  (check-equal? (string-ensure-ends-with-newline "") "\n")
  (check-equal? (string-ensure-ends-with-newline "abc") "abc\n")
  (check-equal? (string-ensure-ends-with-newline "abc\n") "abc\n")
  (check-equal? (string-ensure-ends-with-newline "abc\r\n") "abc\r\n")
  (check-exn exn:fail:contract? (λ () (string-ensure-ends-with-newline 123)))

  ;; string-map
  (check-equal? (string-map char-upcase "abc") "ABC")
  (check-equal? (string-map char-upcase "abcdef" 1 4) "aBCDef")
  (check-equal? (string-map char-upcase "abcdef" -4 -1) "abCDEf")
  (check-equal? (string-map char-upcase "abcdef" 4 2) "abCDef")
  (check-exn exn:fail:contract? (λ () (string-map 123 "abc")))
  (check-exn exn:fail:contract? (λ () (string-map char-upcase 123)))
  (check-exn exn:fail? (λ () (string-map (λ (_ch) 1) "abc")))

  ;; string-map!
  (define s1 (string-copy "abc"))
  (check-equal? (string-map! char-upcase s1) (void))
  (check-equal? s1 "ABC")
  (define s2 (string-copy "abcdef"))
  (check-equal? (string-map! char-upcase s2 1 4) (void))
  (check-equal? s2 "aBCDef")
  (define s3 (string-copy "abcdef"))
  (check-equal? (string-map! char-upcase s3 -4 -1) (void))
  (check-equal? s3 "abCDEf")
  (check-exn exn:fail:contract? (λ () (string-map! char-upcase "abc")))
  (check-exn exn:fail:contract? (λ () (string-map! 123 (string-copy "abc"))))
  (check-exn exn:fail? (λ () (string-map! (λ (_ch) 1) (string-copy "abc"))))

  ;; string-blank?
  (check-true  (string-blank? ""))
  (check-true  (string-blank? " \t\n"))
  (check-false (string-blank? " a "))
  (check-exn exn:fail:contract? (λ () (string-blank? 123)))

  ;; string-ascii?
  (check-true  (string-ascii? ""))
  (check-true  (string-ascii? "ABC123!?"))
  (check-false (string-ascii? "café"))
  (check-exn exn:fail:contract? (λ () (string-ascii? 123)))

  ;; string-digit?
  (check-true  (string-digit? ""))
  (check-true  (string-digit? "0123456789"))
  (check-false (string-digit? "12a3"))
  (check-false (string-digit? "１２３")) ; full-width digits are not ASCII digits
  (check-exn exn:fail:contract? (λ () (string-digit? 123)))

  ;; string-intersperse
  (check-equal? (string-intersperse "," '()) "")
  (check-equal? (string-intersperse "," '("a")) "a")
  (check-equal? (string-intersperse "," '("a" "b" "c")) "a,b,c")
  (check-equal? (string-intersperse "::" '("ab" "cd")) "ab::cd")
  (check-exn exn:fail:contract? (λ () (string-intersperse 1 '("a"))))
  (check-exn exn:fail:contract? (λ () (string-intersperse "," "abc")))
  (check-exn exn:fail:contract? (λ () (string-intersperse "," '("a" 1))))

  ;; string-escape-visible
  (check-equal? (string-escape-visible "") "")
  (check-equal? (string-escape-visible "abc") "abc")
  (check-equal? (string-escape-visible "\n\t\r") "\\n\\t\\r")
  (check-equal? (string-escape-visible "\\x") "\\\\x")
  (check-equal? (string-escape-visible (string #\nul #\rubout)) "\\x00\\x7F")
  (check-exn exn:fail:contract? (λ () (string-escape-visible 123)))

  ;; string-unescape-visible
  (check-equal? (string-unescape-visible "") "")
  (check-equal? (string-unescape-visible "abc") "abc")
  (check-equal? (string-unescape-visible "\\n\\t\\r") "\n\t\r")
  (check-equal? (string-unescape-visible "\\\\x") "\\x")
  (check-equal? (string-unescape-visible "\\x00\\x7F") (string #\nul #\rubout))
  (check-equal? (string-unescape-visible "\\x3BB;") "λ")
  (check-exn exn:fail:contract? (λ () (string-unescape-visible 123)))
  (check-exn exn:fail? (λ () (string-unescape-visible "\\")))
  (check-exn exn:fail? (λ () (string-unescape-visible "\\q")))
  (check-exn exn:fail? (λ () (string-unescape-visible "\\xG1")))

  ;; string-quote / string-unquote
  (check-equal? (string-quote "abc") "\"abc\"")
  (check-equal? (string-quote "a\nb") "\"a\\nb\"")
  (check-equal? (string-quote "a'b" #:quote-char #\') "'a\\'b'")
  (check-equal? (string-unquote "\"a\\nb\"") "a\nb")
  (check-equal? (string-unquote "'a\\'b'" #:quote-char #\') "a'b")
  (check-equal? (string-unquote (string-quote "a\\b\tc")) "a\\b\tc")
  (check-exn exn:fail? (λ () (string-unquote "abc")))
  (check-exn exn:fail? (λ () (string-unquote "\"a\\q\"")))
  (check-exn exn:fail:contract? (λ () (string-quote 123)))

  ;; string-escape-regexp
  (check-equal? (regexp-match? (regexp (string-escape-regexp "a+b")) "a+b") #t)
  (check-equal? (regexp-match? (regexp (string-escape-regexp "a+b")) "ab") #f)
  (check-exn exn:fail:contract? (λ () (string-escape-regexp 123)))

  ;; string-escape-json / string-unescape-json
  (check-equal? (string-escape-json "\"\\/\b\f\n\r\t")
                "\\\"\\\\\\/\\b\\f\\n\\r\\t")
  (check-equal? (string-escape-json (string #\nul #\u001F)) "\\u0000\\u001F")
  (check-equal? (string-unescape-json "\\\"\\\\\\/\\b\\f\\n\\r\\t")
                "\"\\/\b\f\n\r\t")
  (check-equal? (string-unescape-json "\\u0041\\u03BB") "Aλ")
  (check-equal? (string-unescape-json "\\uD83D\\uDE00")
                (string (integer->char #x1F600)))
  (check-equal? (string-unescape-json (string-escape-json "hi λ")) "hi λ")
  (check-exn exn:fail:contract? (λ () (string-escape-json 123)))
  (check-exn exn:fail? (λ () (string-unescape-json "\\u12")))
  (check-exn exn:fail? (λ () (string-unescape-json "\\uD83Dx")))
  (check-exn exn:fail? (λ () (string-unescape-json "\\uDE00")))

  ;; string-strip-ansi
  (check-equal? (string-strip-ansi "") "")
  (check-equal? (string-strip-ansi "plain") "plain")
  (check-equal? (string-strip-ansi "\u001b[31mred\u001b[0m") "red")
  (check-equal? (string-strip-ansi "a\u001b]0;title\u0007b") "ab")
  (check-equal? (string-strip-ansi "a\u001b]8;;https://racket-lang.org\u001b\\link\u001b]8;;\u001b\\b")
                "alinkb")
  (check-exn exn:fail:contract? (λ () (string-strip-ansi 123)))

  ;; string-squeeze
  (check-equal? (string-squeeze "a   b    c" #\space) "a b c")
  (check-equal? (string-squeeze "a\t \n\nb") "a\tb")
  (check-equal? (string-squeeze "baaaana" #\a) "bana")
  (check-equal? (string-squeeze "baaaana" "a") "bana")
  (check-equal? (string-squeeze "aeeiioou" (make-char-set #\a #\e #\i #\o #\u)) "a")
  (check-equal? (string-squeeze "boot queue" (make-char-set #\o #\u #\e)) "bot qu")
  (check-equal? (string-squeeze "112233" char-numeric?) "1")
  (check-exn exn:fail:contract? (λ () (string-squeeze 123)))
  (check-exn exn:fail:contract? (λ () (string-squeeze "abc" (λ (x y) x))))

  ;; string-tokenize
  (check-equal? (string-tokenize "  a  b   c  ") '("a" "b" "c"))
  (check-equal? (string-tokenize "a,b,c" #\,) '("a" "b" "c"))
  (check-equal? (string-tokenize "a,\"b,c\",d" #\, #:quote #\") '("a" "b,c" "d"))
  (check-equal? (string-tokenize "a,b\\,c,d" #\, #:escape #\\) '("a" "b,c" "d"))
  (check-equal? (string-tokenize "abc def ghi" char-whitespace? -7 -1)
                '("def" "gh"))
  (check-exn exn:fail:contract? (λ () (string-tokenize 123)))
  (check-exn exn:fail:contract? (λ () (string-tokenize "a,b" #\, #:quote "x")))
  (check-exn exn:fail? (λ () (string-tokenize "a,\"b" #\, #:quote #\")))

  ;; string-fields
  (check-equal? (string-fields "a,b,,c," #\,) '("a" "b" "" "c" ""))
  (check-equal? (string-fields ",a,b," #\,) '("" "a" "b" ""))
  (check-equal? (string-fields "a,\"b,c\",d" #\, #:quote #\") '("a" "b,c" "d"))
  (check-equal? (string-fields "abcdefgh" #\, #:widths '(2 3 2)) '("ab" "cde" "fg"))
  (check-equal? (string-fields "abcdefgh" #\, #:widths '(2 3 2) #:include-rest? #t)
                '("ab" "cde" "fg" "h"))
  (check-equal? (string-fields "abc" #\, #:widths '(2 3 2)) '("ab" "c" ""))
  (check-exn exn:fail:contract? (λ () (string-fields 123)))
  (check-exn exn:fail:contract? (λ () (string-fields "abc" #\, #:widths '(2 0 1))))

  ;; string-scan
  (check-equal? (let ([g (string-scan "banana" "na")])
                  (let loop ([acc '()])
                    (define v (g))
                    (if v (loop (cons v acc)) (reverse acc))))
                (list (cons 2 4) (cons 4 6)))
  (check-equal? (let ([g (string-scan "aaaa" "aa" #:overlap? #t)])
                  (let loop ([acc '()])
                    (define v (g))
                    (if v (loop (cons v acc)) (reverse acc))))
                (list (cons 0 2) (cons 1 3) (cons 2 4)))
  (check-equal? (let ([g (string-scan "abc123x" char-numeric?)])
                  (let loop ([acc '()])
                    (define v (g))
                    (if v (loop (cons v acc)) (reverse acc))))
                (list (cons 3 4) (cons 4 5) (cons 5 6)))
  (check-equal? (let ([g (string-scan "banana" #\a -5 -1)])
                  (let loop ([acc '()])
                    (define v (g))
                    (if v (loop (cons v acc)) (reverse acc))))
                (list (cons 1 2) (cons 3 4)))
  (check-exn exn:fail:contract? (λ () (string-scan 123 "a")))
  (check-exn exn:fail:contract? (λ () (string-scan "abc" 'x)))

  ;; string-remove-prefix
  (check-equal? (string-remove-prefix "foobar" "foo") "bar")
  (check-equal? (string-remove-prefix "foobar" "bar") "foobar")
  (check-equal? (string-remove-prefix "foo" "") "foo")
  (check-equal? (string-remove-prefix "" "") "")
  (check-exn exn:fail:contract? (λ () (string-remove-prefix 1 "x")))
  (check-exn exn:fail:contract? (λ () (string-remove-prefix "x" 1)))

  ;; string-remove-suffix
  (check-equal? (string-remove-suffix "foobar" "bar") "foo")
  (check-equal? (string-remove-suffix "foobar" "foo") "foobar")
  (check-equal? (string-remove-suffix "foo" "") "foo")
  (check-equal? (string-remove-suffix "" "") "")
  (check-exn exn:fail:contract? (λ () (string-remove-suffix 1 "x")))
  (check-exn exn:fail:contract? (λ () (string-remove-suffix "x" 1)))

  ;; string-ensure-prefix
  (check-equal? (string-ensure-prefix "bar" "foo") "foobar")
  (check-equal? (string-ensure-prefix "foobar" "foo") "foobar")
  (check-equal? (string-ensure-prefix "" "foo") "foo")
  (check-equal? (string-ensure-prefix "foo" "") "foo")
  (check-exn exn:fail:contract? (λ () (string-ensure-prefix 1 "x")))
  (check-exn exn:fail:contract? (λ () (string-ensure-prefix "x" 1)))

  ;; string-ensure-suffix
  (check-equal? (string-ensure-suffix "foo" "bar") "foobar")
  (check-equal? (string-ensure-suffix "foobar" "bar") "foobar")
  (check-equal? (string-ensure-suffix "" "bar") "bar")
  (check-equal? (string-ensure-suffix "bar" "") "bar")
  (check-exn exn:fail:contract? (λ () (string-ensure-suffix 1 "x")))
  (check-exn exn:fail:contract? (λ () (string-ensure-suffix "x" 1)))

  ;; char argument
  (check-equal? (string-count "banana" #\a) 3)
  (check-equal? (string-count "banana" #\z) 0)
  (check-equal? (string-count "banana" "an") 5)

  ;; character-set argument (as a set of characters)
  (check-equal? (string-count "banana" (make-char-set #\a #\n)) 5)

  ;; predicate argument
  (check-equal? (string-count "a1b2c3" char-numeric?) 3)

  ;; start/end bounds
  (check-equal? (string-count "banana" #\a 2 6) 2)
  (check-equal? (string-count "banana" (make-char-set #\a #\n) 1 5) 4)
  (check-equal? (string-count "banana" #\a 3 3) 0)
  (check-equal? (string-count "banana" #\a -5 -1) 2)

  ;; errors
  (check-exn exn:fail:contract? (λ () (string-count 123 #\a)))
  (check-exn exn:fail:contract? (λ () (string-count "abc" (λ (x y) x))))
  (check-equal? (string-count "abc" #\a -1 2) 0)
  (check-equal? (string-count "abc" #\a 1 4) 0)
  (check-equal? (string-count "abc" #\a 2 1) 0)

  ;; string-index
  (check-equal? (string-index "banana" #\a) 1)
  (check-equal? (string-index "banana" #\a 2 6) 3)
  (check-equal? (string-index "banana" #\a -5 -1) 1)
  (check-equal? (string-index "banana" (make-char-set #\n #\z)) 2)
  (check-equal? (string-index "banana" "nz") 2)
  (check-equal? (string-index "a1b2c3" char-numeric?) 1)
  (check-false  (string-index "banana" #\z))
  (check-false  (string-index "banana" #\a 6 6))

  ;; string-index-right
  (check-equal? (string-index-right "banana" #\a) 5)
  (check-equal? (string-index-right "banana" #\a 0 5) 3)
  (check-equal? (string-index-right "banana" #\a -5 -1) 3)
  (check-equal? (string-index-right "banana" (make-char-set #\n #\z)) 4)
  (check-equal? (string-index-right "banana" "nz") 4)
  (check-equal? (string-index-right "a1b2c3" char-numeric?) 5)
  (check-false  (string-index-right "banana" #\z))
  (check-false  (string-index-right "banana" #\a 2 2))

  ;; string-skip
  (check-equal? (string-skip "   abc" #\space) 3)
  (check-equal? (string-skip "   abc" #\space -4 100) 3)
  (check-equal? (string-skip "aaab" #\a) 3)
  (check-equal? (string-skip "aaab" "a") 3)
  (check-equal? (string-skip "aaab" (make-char-set #\a #\b) 0 3) #f)
  (check-equal? (string-skip "123x5" char-numeric?) 3)
  (check-false  (string-skip "aaaa" #\a))

  ;; string-skip-right
  (check-equal? (string-skip-right "abc   " #\space) 2)
  (check-equal? (string-skip-right "abc   " #\space -100 -1) 2)
  (check-equal? (string-skip-right "baaa" #\a) 0)
  (check-equal? (string-skip-right "baaa" "a") 0)
  (check-equal? (string-skip-right "baaa" (make-char-set #\a #\b) 1 4) #f)
  (check-equal? (string-skip-right "5x321" char-numeric?) 1)
  (check-false  (string-skip-right "aaaa" #\a))

  ;; string-trim-left
  (check-equal? (string-trim-left "   abc  ") "abc  ")
  (check-equal? (string-trim-left "aaab" #\a) "b")
  (check-equal? (string-trim-left "aaab" "a") "b")
  (check-equal? (string-trim-left "123x5" char-numeric?) "x5")
  (check-equal? (string-trim-left "abba" (make-char-set #\a #\b)) "")
  (check-equal? (string-trim-left "xxabcxx" #\x 2 7) "abcxx")
  (check-equal? (string-trim-left "xxabcxx" #\x -5 -1) "abcx")

  ;; string-trim-right
  (check-equal? (string-trim-right "   abc  ") "   abc")
  (check-equal? (string-trim-right "baaa" #\a) "b")
  (check-equal? (string-trim-right "baaa" "a") "b")
  (check-equal? (string-trim-right "5x321" char-numeric?) "5x")
  (check-equal? (string-trim-right "abba" (make-char-set #\a #\b)) "")
  (check-equal? (string-trim-right "xxabcxx" #\x 1 6) "xabc")
  (check-equal? (string-trim-right "xxabcxx" #\x -5 -1) "abc")

  ;; trim errors
  (check-exn exn:fail:contract? (λ () (string-trim-left 123)))
  (check-equal? (string-trim-left "abc" #\a -1 2) "")
  (check-exn exn:fail:contract? (λ () (string-trim-right "abc" (λ (x y) x))))
  (check-equal? (string-trim-right "abc" #\a 2 1) "")

  ;; index/skip errors
  (check-exn exn:fail:contract? (λ () (string-index 123 #\a)))
  (check-false  (string-index "abc" #\a -1 2))
  (check-false  (string-index-right "abc" #\a 1 4))
  (check-false  (string-skip "abc" #\a 2 1))
  (check-exn exn:fail:contract? (λ () (string-skip-right "abc" (λ (x y) x)))))
