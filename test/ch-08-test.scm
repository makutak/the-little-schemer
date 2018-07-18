#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-08)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "rember-f-not-curry-test")

(test-equal '(6 2 3)
  (rember-f-not-curry = 5 '(6 2 5 3)))

(test-equal '(beans are good)
  (rember-f-not-curry eq? 'jelly '(jelly beans are good)))

(test-equal '(lemonade and (cake))
  (rember-f-not-curry equal?? '(pop corn) '(lemonade (pop corn) and (cake))))

(test-end "rember-f-not-curry-test")


(test-begin "eq?-salad-test")

(define y 'salad)

(test-equal #t
  (eq?-salad y))

(define y 'tuna)

(test-equal #f
  (eq?-salad y))

(test-end "eq?-salad-test")

(test-begin "eq?-c-test")
(test-equal #f
  ((eq?-c 'salad) 'tuna))
(test-end "eq?-c-test")

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

(test-begin "insert-L-test")
(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-L-test")

(test-begin "insert-R-test")
(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-R-test")

(test-begin "insert-L-not-seqL-test")
(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-L-not-seqL-test")

(test-begin "insert-R-not-seqR-test")
(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insert-R-not-seqR-test")

(test-begin "subst-test")
(test-equal '(ice cream with topping for dessert)
  (subst 'topping 'fudge '(ice cream with fudge for dessert)))
(test-end "subst-test")

(test-begin "rember2-test")
(test-equal '(pizza with and bacon)
  (rember2 'sausage '(pizza with sausage and bacon)))
(test-end "rember2-test")

(test-begin "value-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value y)))

(test-end "value-test")

(test-begin "value2-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value2 x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value2 y)))

(test-end "value2-test")
(test-begin "multirember-f-test")
(test-equal '(shrimp salad salad and)
  ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)))
(test-end "multirember-f-test")


(test-begin "multiremberT-test")
(test-equal '(shrimp salad salad and)
  (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))
(test-end "multiremberT-test")

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

(test-begin "new-friend-test")
(test-equal #f
  (new-frend '() '()))
(test-end "new-friend-test")

(test-begin "latest-friend-test")
(test-equal #f
  ((lambda (newlat seen)
       (latest-friend newlat
                      (cons 'tuna seen))) '() '()))
(test-end "latest-friend-test")

(test-begin "last-friend-test")
(test-equal 3
  (multirember&co 'tuna '(strawberries tuna and swordfish) last-friend))
(test-end "last-friend-test")

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

(test-begin "evens-only*-test")
(test-equal '((2 8) 10 (() 6) 2)
  (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))
(test-end "evens-only*-test")


(test-begin "evens-only*&col-test")
(test-equal '(38 1920 (2 8) 10 (() 6) 2)
  (evens-only*&col '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                   the-last-friend))
(test-end "evens-only*&col-test")
