#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(test-begin "add1-test")
(test-equal 68 (add1 67))
(test-end "add1-test")

(test-begin "sub1-test")
(test-equal 4 (sub1 5))
(test-end "sub1-test")

(test-begin "zero?-test")
(test-equal #t (zero? 0))
(test-equal #f (zero? 1492))
(test-end "zero?-test")

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))

(test-begin "o+-test")
(test-equal 58 (o+ 46 12))
(test-end "o+-test")

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

(test-begin "o--test")
(test-equal 11 (o- 14 3))
(test-equal 8 (o- 17 9))
(test-end "o--test")

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

;;memo: tup -> 数のリストのこと
(test-begin "addtup-test")
(test-equal 18 (addtup '(3 5 2 8)))
(test-equal 43 (addtup '(15 6 7 12 3)))
(test-end "addtup-test")

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (o* n (sub1 m)))))))

(test-begin "o*-test")
(test-equal 36 (o* 12 3))
(test-end "o*-test")

