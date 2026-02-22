#lang info

(define collection "string-tools")
(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib"))
(define scribblings '(("string-tools-lib.scrbl" () (library))))
(define pkg-desc "Practical string utilities")
(define version "0.1")
(define license 'MIT)
(define pkg-authors '("Jens Axel Søgaard"))
