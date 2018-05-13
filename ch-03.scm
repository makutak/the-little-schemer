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

(test-begin "rember-fail-test")

(test-equal '(lettuce and tomato) (rember-fail 'bacon '(bacon lettuce and tomato)))
(test-error '(bacon lettuce tomato) (rember-fail 'and '(bacon lettuce and tomato)))
;;(rember-fail 'and '(bacon lettuce and tomato))
;; ==> (tomato)

(test-end "rember-fail-test")

(define rember
  (lambda (a lat)
    (cond 
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else
      (cons (car lat) (rember a (cdr lat)))))))

(test-begin "rember-pass-test")

(test-equal '(bacon lettuce tomato) (rember 'and '(bacon lettuce and tomato)))
(test-equal '(soy and tomato sauce) (rember 'sauce '(soy sauce and tomato sauce)))

(test-end "rember-pass-test")

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (car l)) (firsts (cdr l)))))))

(test-begin "firsts-test")

(test-equal '(apple plum grape bean) (firsts '((apple peache pumpkin)
                                               (plum pear cherry)
                                               (grape raisin pea)
                                               (bean carrot eggplant))))
(test-equal '(a c e) (firsts '((a c) (c d) (e f))))
(test-equal '() (firsts '()))
(test-equal '(five four eleven) (firsts '((five plumns) (four) (eleven green orange))))
(test-equal '((five plumns) eleven (no)) (firsts '(((five plumns) four)
                                                   (eleven green oranges)
                                                   ((no) more))))


(test-end "firsts-test")

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons old
            (cons new
                  (cdr lat))))
     (else (cons (car lat)
                 (insertR new old (cdr lat)))))))

(test-begin "insertR-test")

(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))
(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insertR-test")

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new
            (cons old
                  (cdr lat))))
     (else (cons (car lat)
                 (insertL new old (cdr lat)))))))

(test-begin "insertL-test")

(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))
(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insertL-test")

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else
      (cons (car lat) (subst new old (cdr lat)))))))

(test-begin "subst-test")
(test-equal '(ice cream with topping for dessert)
  (subst 'topping 'fudge '(ice cream with fudge for dessert)))
(test-end "subst-test")

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else
      (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(test-begin "subst2-test")
(test-equal '(vanilla ice creamwith chocolate topping) (subst2 'vanilla 'chocolate 'banana '(banana ice creamwith chocolate topping)))
(test-end "subst2-test")


(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat)
            (multirember a (cdr lat)))))))

(test-begin "multirember-test")
(test-equal '(coffee tea and hick) (multirember 'cup '(coffee cup tea cup and hick cup)))
(test-end "multirember-test")


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons old
            (cons new (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat) (multiinsertR new old (cdr lat)))))))

(test-begin "multiinsertR-test")
(test-equal '(coffee cup foo tea cup foo and hick cup foo)
  (multiinsertR 'foo 'cup '(coffee cup tea cup and hick cup)))
(test-end "multiinsertR-test")

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new
            (cons old (multiinsertL new old (cdr lat)))))
     (else
      (cons (car lat) (multiinsertL new old (cdr lat)))))))
(test-begin "multiinsertL-test")
(test-equal '(coffee foo cup tea foo cup and hick foo cup)
  (multiinsertL 'foo 'cup '(coffee cup tea cup and hick cup)))
(test-end "multiinsertL-test")

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old)
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))

(test-begin "multisubst-test")
(test-equal '(coffee foo tea foo and hick foo)
    (multisubst 'foo 'cup '(coffee cup tea cup and hick cup)))
(test-end "multisubst-test")
