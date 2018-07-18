#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(define-module (ch-04)
  #:use-module (lib util)
  #:export (addtup
            tup+
            no-nums
            all-nums
            o-length
            occur
            rempick))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

;;memo: tup -> 数のリストのこと
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons
       (o+ (car tup1) (car tup2))
       (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n (o^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else
      (add1 (o/ (o- n m) m))))))

(define o-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (o-length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat)
            (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat))
      (cons (car lat)
            (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else
      (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a)
      (add1 (occur a (cdr lat))))
     (else
      (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons
       (car lat)
       (rempick (sub1 n) (cdr lat)))))))
