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

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons
       (o+ (car tup1) (car tup2))
       (tup+ (cdr tup1) (cdr tup2)))))))

(test-begin "tup+-test")
(test-equal '(11 11 11 11 11) (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
(test-equal '(6 9) (tup+ '(2 3) '(4 6)))
(test-equal '(7 13) (tup+ '(3 7) '(4 6)))
(test-equal '(7 13 8 1) (tup+ '(3 7) '(4 6 8 1)))
(test-end "tup+-test")

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (o> (sub1 n) (sub1 m))))))

(test-begin "o>-test")
(test-equal #f (o> 12 133))
(test-equal #t (o> 120 11))
(test-equal #f (o> 3 3))
(test-end "o>-test")

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))

(test-begin "o<-test")
(test-equal #t (o< 4 6))
(test-equal #f (o< 8 3))
(test-equal #f (o< 6 6))
(test-end "o<-test")

(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(test-begin "o=-test")
(test-equal #t (o= 1 1))
(test-equal #f (o= 1 0))
(test-equal #f (o= 0 1))
(test-end "o=-test")

(define o**
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n (o** n (sub1 m)))))))

(test-begin "o**-test")
(test-equal 1 (o** 1 1))
(test-equal 8 (o** 2 3))
(test-equal 125 (o** 5 3))
(test-equal 1 (o** 100000 0))
(test-end "o**-test")

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else
      (add1 (o/ (o- n m) m))))))

(test-begin "o/-test")
(test-equal 3 (o/ 15 4))
(test-equal 20 (o/ 100 5))
(test-end "o/-test")

(define o-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (o-length (cdr lat)))))))

(test-begin "o-length-test")
(test-equal 6 (o-length '(hotdogs with mustard sauerkraut and pickles)))
(test-equal 5 (o-length '(ham and cheese on rye)))
(test-equal 0 (o-length '()))
(test-end "o-length-test")

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(test-begin "pick-test")
(test-equal 'macaroni (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))
(test-error (pick 0 '(a)))
(test-end "pick-test")

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else
      (cons
       (car lat)
       (rempick (sub1 n) (cdr lat)))))))

(test-begin "rempick-test")
(test-equal '(hotdogs with mustard) (rempick 3 '(hotdogs with hot mustard)))
(test-end"rempick-test")
