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

(test-end "value-test")
