#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember-f-not-curry
  (lambda (test? a l)
    (cond
     ((null? a) '())
     ((test? (car l) a)
      (cdr l))
     (else
      (cons (car l)
            (rember-f-not-curry test? a (cdr l)))))))

(test-begin "rember-f-not-curry-test")

(test-equal '(6 2 3)
  (rember-f-not-curry = 5 '(6 2 5 3)))

(test-equal '(beans are good)
  (rember-f-not-curry eq? 'jelly '(jelly beans are good)))

(test-equal '(lemonade and (cake))
  (rember-f-not-curry equal?? '(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "rember-f-not-curry-test")

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
(test-end "eq?-c-test")


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a)
        (cdr l))
       (else
        (cons (car l)
              ((rember-f test?) a (cdr l))))))))

(test-begin "curry-rember-f-test")

(test-equal '(6 2 3)
  ((rember-f =) 5 '(6 2 5 3)))

(test-equal '(beans are good)
  ((rember-f eq?) 'jelly '(jelly beans are good)))

(test-equal '(lemonade and (cake))
  ((rember-f equal??) '(pop corn) '(lemonade (pop corn) and (cake))))

(test-equal '(shrimp salad and salad)
  ((rember-f eq?) 'tuna '(shrimp salad and tuna salad)))

(test-equal '(equal? eqan? eqlist? eqpair?)
  ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)))

(test-end "curry-rember-f-test")

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons new
              (cons old
                    (cdr l))))
       (else
        (cons (car l)
              ((insertL-f test?) new old (cdr l))))))))

(test-begin "insertL-f-test")

(test-equal '(ice cream with topping fudge for dessert)
  ((insertL-f eq?) 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  ((insertL-f eq?) 'jalapeno 'and '(tacos tamales and sals)))

(test-equal '(2 3 4 99 1 10)
  ((insertL-f =) 99 1 '(2 3 4 1 10)))

(test-equal '(2 3 4 99 1 10)
  ((insertL-f =) 99 1 '(2 3 4 1 10)))

(test-equal '(lemonade (foo bar) (pop corn) and (cake))
  ((insertL-f equal??) '(foo bar)'(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "insertL-f-test")

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons old
              (cons new
                    (cdr l))))
       (else
        (cons (car l)
              ((insertR-f test?) new old (cdr l))))))))

(test-begin "insertR-f-test")

(test-equal '(ice cream with fudge topping for dessert)
  ((insertR-f eq?) 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  ((insertR-f eq?) 'jalapeno 'and '(tacos tamales and sals)))

(test-equal '(2 3 4 1 99 10)
  ((insertR-f =) 99 1 '(2 3 4 1 10)))

(test-equal '(lemonade (pop corn) (foo bar) and (cake))
  ((insertR-f equal??) '(foo bar)'(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "insertR-f-test")

;;上記関数は重複している箇所が多いので、まとめたい。
;;どこに挿入するかの箇所だけが違う。
;; new old l を受取り、適切なconsを実行する関数を作る。

(define seqL
  (lambda (new old l)
    (cons new
          (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old
          (cons new l))))

;;これらの関数を受け取る関数を作る。

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else
        (cons (car l)
              ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g seqL))

(test-begin "insert-L-test")
(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-L-test")


(define insertR
  (insert-g seqR))

(test-begin "insert-R-test")
(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-R-test")

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new
           (cons  old
                  l)))))

(test-begin "insert-L-not-seqL-test")
(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-L-not-seqL-test")

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old
           (cons new
                 l)))))

(test-begin "insert-R-not-seqR-test")
(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-R-not-seqR-test")

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(test-begin "subst-test")
(test-equal '(ice cream with topping for dessert)
  (subst 'topping 'fudge '(ice cream with fudge for dessert)))
(test-end "subst-test")
