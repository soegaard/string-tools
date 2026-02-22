#lang info

(define deps '("base" "scribble-lib" "string-tools-lib"))
(define build-deps '("racket-doc"))
(define scribblings '(("string-tools.scrbl" () (library))))
(define pkg-desc "Documentation for string-tools")
(define version "0.1")
(define license 'MIT)
(define pkg-authors '("Jens Axel Søgaard"))
