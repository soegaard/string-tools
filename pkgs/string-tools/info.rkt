#lang info

(define collection  "string-tools-doc")
(define deps        '("base" "scribble-lib" "string-tools-lib"))
(define build-deps  '("racket-doc"))
(define scribblings '(("string-tools.scrbl" () (parsing-library))))
(define pkg-desc    "Documentation for string-tools and user-facing API guide.")
(define pkg-tags    '("string" "strings" "text" "documentation"))
(define version     "0.1")
(define license     'MIT)
(define pkg-authors '("Jens Axel Søgaard"))
