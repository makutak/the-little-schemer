#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)


;; memo: 算術式とは
;; 数を含むアトムか、
;; 2つの算術式を o+, o*, o^ で結合したものである。

;; 算術式の表現が、
;; o+, o*, o^ を除いて数だけを含んでいるか
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) 'o+)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o*)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o^)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(test-begin "numbered?-test")

(let ((x 1))
  (test-equal #t
    (numbered? x)))

(let ((x '(1 o+ 1)))
  (test-equal #t
    (numbered? x)))

(let ((y '(3 o+ (4 o^ 5))))
  (test-equal #t
    (numbered? y)))

(let ((y '(2 o* sausage)))
  (test-equal #f
    (numbered? y)))

(test-end "numbered?-test")

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'o+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o*)
      (o* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     (else
      (o^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))

(test-begin "value-test")

(let ((u 13))
  (test-equal 13
    (value 13)))

(let ((x '(1 o+ 3)))
  (test-equal 4
    (value x)))

(let ((y '(1 o+ (3 o^ 4))))
  (test-equal 82
    (value y)))

(let ((z 'cookie))
  (test-equal 'cookie
   (value z)))

(let ((a '(1 o+ (3 o* 4))))
  (test-equal 13
    (value a)))

(let ((b '((1 o+ 3) o* 4)))
  (test-equal 16
    (value b)))

(test-end "value-test")

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


(define value-prefix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     (else
      (o^ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp)))))))

(test-begin "value-prefix-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value-prefix x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value-prefix y)))

(test-end "value-prefix-test")

(define sero?
  (lambda (n)
    (null? n)))

(test-begin "sero?-test")
(test-equal #t (sero? '()))
(test-equal #f (sero? '(())))
(test-end "sero?-test")

(define edd1
  (lambda (n)
    (cons '() n)))

(test-begin "edd1-test")

(test-equal '(())
  (edd1 '()))

(test-equal '(() () ())
  (edd1 '(() ())))

(test-equal '(() ())
  (edd1 '(())))

(test-end "edd1-test")

(define zub1
  (lambda (n)
    (cdr n)))

(test-begin "zub1-test")

(test-error (zub1 '()))

(test-equal '(())
  (zub1 '(() ())))

(test-equal '(() ())
  (zub1 '(() () ())))

(test-end "zub1-test")

(define .+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else
      (edd1 (.+ n (zub1 m)))))))

(test-begin ".+-test")

(test-equal '(() ())
  (.+ '(()) '(())))

(test-equal '(() () ())
  (.+ '(()) '(() ())))

(test-end ".+-test")

(test-begin "new-number-lat?-test")
(let ((ls '((()) (() ()) (() () ()))))
  (test-equal #f
    (lat? ls)))
(test-end "new-number-lat?-test")
