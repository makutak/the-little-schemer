#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember-fail
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a) (cdr lat))
       (else (rember a (cdr lat))))))))

(test-begin "ch-03-fail-test")

(test-equal '(lettuce and tomato) (rember-fail 'bacon '(bacon lettuce and tomato)))
(test-error '(bacon lettuce tomato) (rember-fail 'and '(bacon lettuce and tomato)))
;;(rember-fail 'and '(bacon lettuce and tomato))
;; ==> (tomato)

(test-end "ch-03-fail-test")

(define rember
  (lambda (a lat)
    (cond 
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a) (cdr lat))
       (else
        (cons (car lat) (rember a (cdr lat)))))))))

(test-begin "ch-03-pass-test")

(test-equal '(bacon lettuce tomato) (rember 'and '(bacon lettuce and tomato)))

(test-end "ch-03-pass-test")
