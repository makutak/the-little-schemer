#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? sorn a)))))

(test-begin "keep-looking-test")

(test-equal #t
  (keep-looking 'caviar
                3
                '(6 2 4 caviar 5 7 3)))

(test-end "keep-looking-test")

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(test-begin "looking-test")

(test-equal #t
  (looking 'caviar
           '(6 2 4 caviar 5 7 3)))

(test-equal #f
  (looking 'caviar
           '(6 2 grits caviar 5 7 3)))

(test-equal #t
  (looking 'caviar
           '(6 2 4 caviar 5 7 3)))

(test-end "looking-test")

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(test-begin "shift-test")

(test-equal '(a (b c))
  (shift '((a b) c)))

(test-equal '(a (b (c d)))
  (shift '((a b) (c d))))

(test-end "shift-test")
