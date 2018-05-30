#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? a) '())
     ((test? (car l) a)
      (cdr l))
     (else
      (cons (car l)
            (rember-f test? a (cdr l)))))))

(test-begin "rember-f-test")

(test-equal '(6 2 3)
  (rember-f = 5 '(6 2 5 3)))

(test-equal '(beans are good)
  (rember-f eq? 'jelly '(jelly beans are good)))

(test-equal '(lemonade and (cake))
  (rember-f equal?? '(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "rember-f-test")

;; memo:
;;
;; (lambda (a)
;;   (lambda (x)
;;     (eq? x a)))
;; => 引数としてaを渡されると、
;;    関数 (lambda (x)
;;           (eq? x a))
;;    を返す
;;    => カリー化と言う。


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;;kに'saladと名付ける
(define k 'salad)

;;(eq?-c k)という処理にeq?-saladと名付ける
(define eq?-salad (eq?-c k))

(test-begin "eq?-salad-test")

(define y 'salad)

(test-equal #t
  (eq?-salad y))

(define y 'tuna)

(test-equal #f
  (eq?-salad y))

(test-end "eq?-salad-test")

;;上記のことは、下記でできる
(let ((x 'salad)
      (y 'tuna))
  ((eq?-c x) y))

(test-begin "eq?-c-test")
(test-equal #f
  ((eq?-c 'salad) 'tuna))
(test-begin "eq?-c-test")
