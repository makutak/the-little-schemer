#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-03)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "rember-fail-test")

(test-equal '(lettuce and tomato)
  (rember-fail 'bacon '(bacon lettuce and tomato)))

(test-equal '(tomato)
            (rember-fail 'and '(bacon lettuce and tomato)))

(test-end "rember-fail-test")


(test-begin "rember-pass-test")

(test-equal '(bacon lettuce tomato)
  (rember 'and '(bacon lettuce and tomato)))

(test-equal '(soy and tomato sauce)
  (rember 'sauce '(soy sauce and tomato sauce)))

(test-end "rember-pass-test")


(test-begin "firsts-test")

(test-equal '(apple plum grape bean)
  (firsts '((apple peache pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant))))

(test-equal '(a c e)
  (firsts '((a c) (c d) (e f))))

(test-equal '()
  (firsts '()))

(test-equal '(five four eleven)
  (firsts '((five plumns) (four) (eleven green orange))))

(test-equal '((five plumns) eleven (no))
  (firsts '(((five plumns) four)
            (eleven green oranges)
            ((no) more))))

(test-end "firsts-test")

(test-begin "insertR-test")

(test-equal '(ice cream with fudge topping for dessert)
  (insertR 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales and jalapeno sals)
  (insertR 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insertR-test")


(test-begin "insertL-test")

(test-equal '(ice cream with topping fudge for dessert)
  (insertL 'topping 'fudge '(ice cream with fudge for dessert)))

(test-equal '(tacos tamales jalapeno and sals)
  (insertL 'jalapeno 'and '(tacos tamales and sals)))

(test-end "insertL-test")

(test-begin "subst-test")

(test-equal '(ice cream with topping for dessert)
  (subst 'topping 'fudge '(ice cream with fudge for dessert)))

(test-end "subst-test")

(test-begin "subst2-test")

(test-equal '(vanilla ice creamwith chocolate topping)
  (subst2 'vanilla 'chocolate 'banana '(banana ice creamwith chocolate topping)))

(test-end "subst2-test")

(test-begin "multirember-test")

(test-equal '(coffee tea and hick)
  (multirember 'cup '(coffee cup tea cup and hick cup)))

(test-end "multirember-test")


(test-begin "multiinsertR-test")

(test-equal '(coffee cup foo tea cup foo and hick cup foo)
  (multiinsertR 'foo 'cup '(coffee cup tea cup and hick cup)))

(test-end "multiinsertR-test")

(test-begin "multiinsertL-test")

(test-equal '(coffee foo cup tea foo cup and hick foo cup)
  (multiinsertL 'foo 'cup '(coffee cup tea cup and hick cup)))

(test-end "multiinsertL-test")

(test-begin "multisubst-test")

(test-equal '(coffee foo tea foo and hick foo)
  (multisubst 'foo 'cup '(coffee cup tea cup and hick cup)))

(test-end "multisubst-test")
