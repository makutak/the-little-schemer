#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember-f
  (lambda (test? a l)
    "rember-f"))

(test-begin "rember-f-test")

(test-equal '(6 2 3)
  (rember-f = 5 '(6 2 5 3)))

(test-equal '(beans are good)
  (rember-f eq? 'jelly '(jelly beans are good)))

(test-equal '(lemonade (pop corn) and (cake))
  (rember-f equal?? '(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "rember-f-test")
