#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

(test-begin "set?-test")

(let ((lat '(apple peaches apple plum)))
  (test-equal #f
    (set? lat)))

(let ((lat '(apples peaches pears plums)))
  (test-equal #t
    (set? lat)))

(let ((lat '()))
  (test-equal #t
    (set? lat)))

(let ((lat '(apple 3 pear 4 9 apple 3 4)))
  (test-equal #f
    (set? lat)))

(test-end "set?-test")
