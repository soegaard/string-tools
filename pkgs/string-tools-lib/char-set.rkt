#lang racket/base

(require racket/list
         racket/vector)

(provide char-set?
         empty-char-set
         make-char-set
         list->char-set
         string->char-set
         char-set-add
         char-set-add-range
         char-set-member?
         char-set-union
         char-set-intersection
         char-set-difference
         char-set-size)

;; Character-set representation:
;;   A character set is stored as a hybrid of:
;;   1) an ASCII bit mask (codepoints 0..127), and
;;   2) normalized non-ASCII ranges as inclusive integer intervals.
;;
;; Why this representation:
;;   ASCII membership is extremely fast with bit operations, which matches the
;;   common case for many workloads. Non-ASCII characters are stored compactly
;;   as merged, sorted ranges, which keeps Unicode-heavy sets space-efficient
;;   while still allowing fast membership tests via binary search.
;;
;; Hybrid representation:
;; - ascii-mask: bit i set => codepoint i (0..127) is in the set
;; - ranges: vector of (cons lo hi), lo/hi are inclusive codepoints >= 128,
;;           sorted, non-overlapping, and non-adjacent.
(struct char-set (ascii-mask ranges)
  #:transparent
  #:constructor-name make-char-set/internal)

;; empty-char-set : char-set?
;;   The empty character set.
(define empty-char-set
  (make-char-set/internal 0 #()))

;; check-char : symbol? any/c -> void?
;;   Raise a contract error unless x is a character.
(define (check-char who x)
  (unless (char? x)
    (raise-argument-error who "char?" x)))

;; check-char-set : symbol? any/c -> void?
;;   Raise a contract error unless x is a char-set value.
(define (check-char-set who x)
  (unless (char-set? x)
    (raise-argument-error who "char-set?" x)))

;; ascii-member? : exact-nonnegative-integer? exact-nonnegative-integer? -> boolean?
;;   Check whether ASCII codepoint cp is present in mask.
(define (ascii-member? mask cp)
  (and (< cp 128)
       (not (zero? (bitwise-and mask (arithmetic-shift 1 cp))))))

;; ascii-add : exact-nonnegative-integer? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Add ASCII codepoint cp to mask and return the updated mask.
(define (ascii-add mask cp)
  (if (< cp 128)
      (bitwise-ior mask (arithmetic-shift 1 cp))
      mask))

;; range-lo : (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) -> exact-nonnegative-integer?
;;   Return the lower bound of an inclusive codepoint range.
(define (range-lo r) (car r))
;; range-hi : (cons/c exact-nonnegative-integer? exact-nonnegative-integer?) -> exact-nonnegative-integer?
;;   Return the upper bound of an inclusive codepoint range.
(define (range-hi r) (cdr r))

;; sort-and-merge-ranges : (listof (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)) -> vector?
;;   Sort ranges and merge overlaps/adjacent intervals.
(define (sort-and-merge-ranges rs)
  (cond
    [(null? rs) #()]
    [else
     (define sorted (sort rs < #:key range-lo))
     (define merged
       (for/fold ([out '()]) ([r (in-list sorted)])
         (cond
           [(null? out) (list r)]
           [else
            (define prev (car out))
            (define plo  (range-lo prev))
            (define phi  (range-hi prev))
            (define rlo  (range-lo r))
            (define rhi  (range-hi r))
            (if (<= rlo (add1 phi))
                (cons (cons plo (max phi rhi)) (cdr out))
                (cons r out))])))
     (list->vector (reverse merged))]))

;; ranges-member? : vector? exact-nonnegative-integer? -> boolean?
;;   Check whether cp occurs in a sorted vector of inclusive ranges.
(define (ranges-member? ranges cp)
  (let loop ([lo 0] [hi (sub1 (vector-length ranges))])
    (cond
      [(> lo hi) #f]
      [else
       (define mid (quotient (+ lo hi) 2))
       (define r   (vector-ref ranges mid))
       (define rlo (range-lo r))
       (define rhi (range-hi r))
       (cond
         [(< cp rlo) (loop lo (sub1 mid))]
         [(> cp rhi) (loop (add1 mid) hi)]
         [else #t])])))

;; range-intersection
;;   : (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
;;     (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
;;     -> (or/c #f (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))
;;   Return the overlap of two inclusive ranges, or #f if disjoint.
(define (range-intersection a b)
  (define alo (range-lo a))
  (define ahi (range-hi a))
  (define blo (range-lo b))
  (define bhi (range-hi b))
  (define lo  (max alo blo))
  (define hi  (min ahi bhi))
  (and (<= lo hi) (cons lo hi)))

;; range-difference
;;   : (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
;;     (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
;;     -> (listof (cons/c exact-nonnegative-integer? exact-nonnegative-integer?))
;;   Subtract range b from range a, yielding zero, one, or two ranges.
(define (range-difference a b)
  ;; Returns 0, 1, or 2 ranges as a list.
  (define alo (range-lo a))
  (define ahi (range-hi a))
  (define blo (range-lo b))
  (define bhi (range-hi b))
  (cond
    [(or (> blo ahi) (> alo bhi)) (list a)]
    [else
     (append
      (if (< alo blo) (list (cons alo (sub1 blo))) '())
      (if (< bhi ahi) (list (cons (add1 bhi) ahi)) '()))]))

;; vector->list/ranges : vector? -> list?
;;   Convert a range vector to a list preserving order.
(define (vector->list/ranges v)
  (for/list ([i (in-range (vector-length v))])
    (vector-ref v i)))

;; ranges-union : vector? vector? -> vector?
;;   Compute the union of two normalized range vectors.
(define (ranges-union ra rb)
  (define la (vector->list/ranges ra))
  (define lb (vector->list/ranges rb))
  (sort-and-merge-ranges (append la lb)))

;; ranges-intersection : vector? vector? -> vector?
;;   Compute the intersection of two normalized range vectors.
(define (ranges-intersection ra rb)
  (define a-len (vector-length ra))
  (define b-len (vector-length rb))
  (let loop ([i 0] [j 0] [out '()])
    (cond
      [(or (>= i a-len) (>= j b-len))
       (list->vector (reverse out))]
      [else
       (define a        (vector-ref ra i))
       (define b        (vector-ref rb j))
       (define hit      (range-intersection a b))
       (define next-out (if hit (cons hit out) out))
       (cond
         [(< (range-hi a) (range-hi b)) (loop (add1 i) j next-out)]
         [else (loop i (add1 j) next-out)])])))

;; ranges-difference : vector? vector? -> vector?
;;   Compute range-vector difference ra - rb.
(define (ranges-difference ra rb)
  ;; ra - rb
  (define la (vector->list/ranges ra))
  (define lb (vector->list/ranges rb))
  ;; subtract-range : range (listof range) -> (listof range)
  ;;   Subtract all ranges in bs from a.
  (define (subtract-range a bs)
    (let loop ([parts (list a)] [bs bs])
      (cond
        [(null? parts) '()]
        [(null? bs) parts]
        [else
         (define b     (car bs))
         (define p-min (range-lo (car parts)))
         (define p-max (range-hi (last parts)))
         (cond
           [(> (range-lo b) p-max) parts]
           [(< (range-hi b) p-min) (loop parts (cdr bs))]
           [else
            (define next-parts
              (append-map (λ (p) (range-difference p b)) parts))
            (loop next-parts (cdr bs))])])))
  (sort-and-merge-ranges
   (append-map (λ (a) (subtract-range a lb)) la)))

;; list->char-set : (listof char?) -> char-set?
;;   Build a character set from a list of characters.
(define (list->char-set xs)
  (define-values (mask ranges)
    (for/fold ([mask 0] [ranges '()]) ([x (in-list xs)])
      (check-char 'list->char-set x)
      (define cp (char->integer x))
      (if (< cp 128)
          (values (ascii-add mask cp) ranges)
          (values mask (cons (cons cp cp) ranges)))))
  (make-char-set/internal mask (sort-and-merge-ranges ranges)))

;; string->char-set : string? -> char-set?
;;   Build a character set from the distinct characters in a string.
(define (string->char-set s)
  (unless (string? s)
    (raise-argument-error 'string->char-set "string?" s))
  (list->char-set (string->list s)))

;; make-char-set : char? ... -> char-set?
;;   Build a character set from zero or more character arguments.
(define (make-char-set . chars)
  (list->char-set chars))

;; char-set-add : char-set? char? -> char-set?
;;   Return a character set containing all characters of cs and ch.
(define (char-set-add cs ch)
  (check-char-set 'char-set-add cs)
  (check-char 'char-set-add ch)
  (define cp (char->integer ch))
  (if (< cp 128)
      (make-char-set/internal
       (ascii-add (char-set-ascii-mask cs) cp)
       (char-set-ranges cs))
      (make-char-set/internal
       (char-set-ascii-mask cs)
       (sort-and-merge-ranges
        (cons (cons cp cp)
              (vector->list/ranges (char-set-ranges cs)))))))

;; char-set-add-range : char-set? char? char? -> char-set?
;;   Return a character set containing cs plus all characters in [lo-ch, hi-ch].
(define (char-set-add-range cs lo-ch hi-ch)
  (check-char-set 'char-set-add-range cs)
  (check-char 'char-set-add-range lo-ch)
  (check-char 'char-set-add-range hi-ch)
  (define lo (char->integer lo-ch))
  (define hi (char->integer hi-ch))
  (unless (<= lo hi)
    (raise-arguments-error 'char-set-add-range
                           "expected lo <= hi"
                           "lo" lo-ch
                           "hi" hi-ch))
  (define mask0   (char-set-ascii-mask cs))
  (define ranges0 (vector->list/ranges (char-set-ranges cs)))
  (define mask1
    (if (< lo 128)
        (for/fold ([mask mask0]) ([cp (in-range lo (add1 (min hi 127)))])
          (ascii-add mask cp))
        mask0))
  (define ranges1
    (if (> hi 127)
        (cons (cons (max 128 lo) hi) ranges0)
        ranges0))
  (make-char-set/internal mask1 (sort-and-merge-ranges ranges1)))

;; char-set-member? : char-set? char? -> boolean?
;;   Check whether ch is a member of cs.
(define (char-set-member? cs ch)
  (check-char-set 'char-set-member? cs)
  (check-char 'char-set-member? ch)
  (define cp (char->integer ch))
  (if (< cp 128)
      (ascii-member? (char-set-ascii-mask cs) cp)
      (ranges-member? (char-set-ranges cs) cp)))

;; char-set-union : char-set? char-set? -> char-set?
;;   Return the union of two character sets.
(define (char-set-union a b)
  (check-char-set 'char-set-union a)
  (check-char-set 'char-set-union b)
  (make-char-set/internal
   (bitwise-ior (char-set-ascii-mask a)
                (char-set-ascii-mask b))
   (ranges-union (char-set-ranges a)
                 (char-set-ranges b))))

;; char-set-intersection : char-set? char-set? -> char-set?
;;   Return the intersection of two character sets.
(define (char-set-intersection a b)
  (check-char-set 'char-set-intersection a)
  (check-char-set 'char-set-intersection b)
  (make-char-set/internal
   (bitwise-and (char-set-ascii-mask a)
                (char-set-ascii-mask b))
   (ranges-intersection (char-set-ranges a)
                        (char-set-ranges b))))

;; char-set-difference : char-set? char-set? -> char-set?
;;   Return the set difference a - b.
(define (char-set-difference a b)
  (check-char-set 'char-set-difference a)
  (check-char-set 'char-set-difference b)
  (make-char-set/internal
   (bitwise-and (char-set-ascii-mask a)
                (bitwise-not (char-set-ascii-mask b)))
   (ranges-difference (char-set-ranges a)
                      (char-set-ranges b))))

;; bit-count : exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Count set bits in n.
(define (bit-count n)
  ;; Brian Kernighan popcount
  (let loop ([x n] [k 0])
    (if (zero? x)
        k
        (loop (bitwise-and x (sub1 x)) (add1 k)))))

;; char-set-size : char-set? -> exact-nonnegative-integer?
;;   Count the number of characters in cs.
(define (char-set-size cs)
  (check-char-set 'char-set-size cs)
  (+ (bit-count (char-set-ascii-mask cs))
     (for/sum ([r (in-vector (char-set-ranges cs))])
       (add1 (- (range-hi r) (range-lo r))))))

(module+ test
  (require rackunit)

  (define ascii (make-char-set #\a #\z #\a))
  (check-true (char-set-member? ascii #\a))
  (check-false (char-set-member? ascii #\b))
  (check-equal? (char-set-size ascii) 2)

  (define cs1 (char-set-add-range empty-char-set #\a #\f))
  (check-true (char-set-member? cs1 #\a))
  (check-true (char-set-member? cs1 #\d))
  (check-false (char-set-member? cs1 #\z))
  (check-equal? (char-set-size cs1) 6)

  (define uni1 (char-set-add-range empty-char-set #\u03B1 #\u03B4)) ; α..δ
  (check-true (char-set-member? uni1 #\u03B2))
  (check-false (char-set-member? uni1 #\u03B6))

  (define both (char-set-union cs1 uni1))
  (check-true (char-set-member? both #\b))
  (check-true (char-set-member? both #\u03B3))
  (check-equal? (char-set-size both) 10)

  (define i1 (char-set-intersection
              (char-set-add-range empty-char-set #\a #\m)
              (char-set-add-range empty-char-set #\h #\z)))
  (check-true (char-set-member? i1 #\h))
  (check-true (char-set-member? i1 #\m))
  (check-false (char-set-member? i1 #\g))
  (check-equal? (char-set-size i1) 6)

  (define d1 (char-set-difference
              (char-set-add-range empty-char-set #\a #\j)
              (char-set-add-range empty-char-set #\d #\f)))
  (check-true (char-set-member? d1 #\a))
  (check-false (char-set-member? d1 #\e))
  (check-true (char-set-member? d1 #\j))
  (check-equal? (char-set-size d1) 7)

  (check-equal? (char-set-size (string->char-set "banana")) 3)
  (check-equal? (char-set-size (list->char-set (list #\x #\x #\y))) 2)

  (check-exn exn:fail:contract? (λ () (list->char-set (list 'x))))
  (check-exn exn:fail:contract? (λ () (string->char-set 123)))
  (check-exn exn:fail:contract? (λ () (char-set-add empty-char-set 'x)))
  (check-exn exn:fail:contract? (λ () (char-set-member? empty-char-set 1)))
  (check-exn exn:fail:contract?
             (λ () (char-set-add-range empty-char-set #\z #\a))))
