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

(test-equal #t
  (keep-looking 'caviar
                4
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

(define align
  (lambda (para)
    (cond
     ((atom? para)
      para)
     ((a-pair? (first para))
      (align (shift para)))
     (else
      (build (first para)
             (align (second para)))))))
;;=> cond でshiftはalignのために引数を生成するが、それは元の引数の一部分ではない。

(define length*
  (lambda (para)
    (cond
     ((atom? para) 1)
     (else
      (o+ (length* (first para))
          (length* (second para)))))))

(define weight*
  (lambda (para)
    (cond
     ((atom? para) 1)
     (else
      (o+ (o* (weight* (first para)) 2)
          (weight* (second para)))))))

(test-begin "weight*-test")

(test-equal 7
  (weight* '((a b) c)))

(test-equal 5
  (weight* '(a (b c))))

(test-end "weight*-test")

(define shuffle
  (lambda (para)
    (cond
     ((atom? para)
      para)
     ((a-pair? (first para))
      (shuffle (revpair para)))
     (else
      (build (first para)
             (shuffle (second para)))))))

(test-begin "shuffle-test")

(test-equal '(a (b c))
  (shuffle '(a (b c))))

(test-equal '(a b)
  (shuffle '(a b)))

(test-end "shuffle-test")

;; (shuffle '((a b) (c d)))
;; => (revpair '((a b) (c d)))
;; ==> '((c d) (a b))
;; となり、循環する?
;; 以下textより、
;; ```
;; この値を決めるためには、(shuffle (revpair para)) が何かを見つけなければいけない
;; (revpair para) が ((a b) (c d)) のときの (shuffle (revpair para)) の値を知る必要がある。
;;```

(define C
  (lambda (n)
    (cond
     ((one? n)
      1)
     (else
      (cond
       ((even?? n)
        (C (o/ n 2)))
       (else
        (C (add1 (o* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n)
      (add1 m))
     ((zero? m)
      (A (sub1 n) 1))
     (else
      (A (sub1 n)
         (A n (sub1 m)))))))

(test-begin "A-test")

(test-equal 2
  (A 1 0))

(test-equal 3
  (A 1 1))

(test-equal 7
  (A 2 2))

(test-end "A-test")

(define my-length
  (lambda (l)
    (cond
     ((null? l)
      0)
     (else
      (add1 (my-length (cdr l)))))))
