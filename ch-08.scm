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

(define seqrem
  (lambda (new old l)
    l))

(define rember2
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(test-begin "rember2-test")
(test-equal '(pizza with and bacon)
  (rember2 'sausage '(pizza with sausage and bacon)))
(test-end "rember2-test")

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     (else
      (o^ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(test-begin "value-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value y)))

(test-end "value-test")

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x 'o+) o+)
     ((eq? x 'o*) o*)
     (else o^))))

(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
        (operator nexp))
       (value2 (1st-sub-exp nexp))
       (value2 (2nd-sub-exp nexp)))))))

(test-begin "value2-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value2 x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value2 y)))

(test-end "value2-test")

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
        ((multirember-f test?) a (cdr lat)))
       (else
        (cons (car lat)
              ((multirember-f test?) a  (cdr lat))))))))

(test-begin "multirember-f-test")
(test-equal '(shrimp salad salad and)
  ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)))
(test-end "multirember-f-test")

(define multirember-eq
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else
      (cons (car lat)
            (multiremberT test? (cdr lat)))))))

(test-begin "multiremberT-test")
(test-equal '(shrimp salad salad and)
  (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))
(test-end "multiremberT-test")

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(test-begin "a-friend-test")
(test-equal #f
  (multirember&co 'tuna
                  '(strawberries tuna and swordfish)
                  a-friend))
(test-equal #t
  (multirember&co 'tuna
                  '()
                  a-friend))

(test-equal #f
  (multirember&co 'tuna
                  '(tuna)
                  a-friend))
(test-end "a-friend-test")

(define new-frend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(test-begin "new-friend-test")
(test-equal #f
  (new-frend '() '()))
(test-end "new-friend-test")

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
              seen)))

(test-begin "latest-friend-test")
(test-equal #f
  ((lambda (newlat seen)
       (latest-friend newlat
                      (cons 'tuna seen))) '() '()))
(test-end "latest-friend-test")

(define last-friend
  (lambda (x y)
    (length x)))

(test-begin "last-friend-test")
(test-equal 3
  (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
(test-end "last-friend-test")

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR (cdr lat)))))))

(test-begin "multiinsertLR-test")

(test-equal '(foo coffee cup tea foo cup and hick cup)
  (multiinsertLR 'foo 'coffee 'tea '(coffee cup tea cup and hick cup)))

(test-equal '(coffee cup foo tea cup foo and hick cup foo)
  (multiinsertLR 'foo 'hoge 'cup '(coffee cup tea cup and hick cup)))

(test-equal '(coffee foo cup tea foo cup and hick foo cup)
  (multiinsertLR 'foo 'cup 'hoge '(coffee cup tea cup and hick cup)))

(test-equal '(coffee cup tea cup and hick cup)
  (multiinsertLR 'foo 'hoge 'fuga '(coffee cup tea cup and hick cup)))

(test-end "multiinsertLR-test")

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
     (else
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L
                                R)))))))

(test-begin "multiinsertLR*co-test")

;;新しく作られたリストを返す
(test-equal '(chips salty and salty fish or salty fish and chips salty)
  (multiinsertLR&co 'salty
                    'fish
                    'chips
                    '(chips and fish or fish and chips)
                    (lambda (newlat L R) newlat)))

;;newがoldLに一致した回数を返す
(test-equal 2
  (multiinsertLR&co 'salty
                    'fish
                    'chips
                    '(chips and fish or fish and chips)
                    (lambda (newlat L R) L)))

;;newがoldRに一致した回数を返す
(test-equal 2
  (multiinsertLR&co 'salty
                    'fish
                    'chips
                    '(chips and fish or fish and chips)
                    (lambda (newlat L R) R)))

(test-end "multiinsertLR*co-test")

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even?? (car l))
        (cons (car l)
              (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))

(test-begin "evens-only*-test")
(test-equal '((2 8) 10 (() 6) 2)
  (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))
(test-end "evens-only*-test")

(define evens-only*&col
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car  l))
      (cond
       ((even?? (car l))
        (evens-only*&col (cdr l)
                         (lambda (newl p s)
                           (col (cons (car l) newl)
                                (o* (car l) p) s))))
       (else
        (evens-only*&col (cdr l)
                         (lambda (newl p s)
                           (col newl
                                p (o+ (car l) s)))))))
     (else
      (evens-only*&col (car l)
                       (lambda (al ap as)
                         (evens-only*&col (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (o* ap dp)
                                                 (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

(test-begin "evens-only*&col-test")
(test-equal '(38 1920 (2 8) 10 (() 6) 2)
  (evens-only*&col '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                   the-last-friend))
(test-end "evens-only*&col-test")
