#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define looking
  (lambda (a lat)
    #t))

(test-begin "looking-test")
(test-equal #t
  (looking 'caviar
           '(6 2 4 caviar 5 7 3)))

(test-equal #f
  (looking 'caviar
           '(6 2 grits caviar 5 7 3)))
(test-end "looking-test")