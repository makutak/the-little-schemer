#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define numbered?
  (lambda (aexp)
    #t))

(test-begin "numbered?-test")

(let ((x 1))
  (test-equal #t
    (numbered? x)))

(let ((y '(3 o+ (4 ^ 5))))
  (test-equal #t
    (numbered? y)))

(let ((y '(2 o* sausage)))
  (test-equal #f
    (numbered? y)))

(test-end "numbered?-test")
