#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-05)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "rember*-test")
(test-equal '((coffee) ((tea))
              (and (hick)))
  (rember* 'cup
           '((coffee) cup ((tea) cup)
             (and (hick)) cup)))

(test-equal '(((tomato))
              ((bean))
              (and ((flying))))
  (rember* 'sauce '(((tomato sauce))
                   ((bean) sauce)
                   (and ((flying)) sauce))))
(test-end "rember*-test")

(test-begin "insertR*-test")
(test-equal '((how much (wood))
              could
              ((a (wood) chuck roast))
              (((chuck roast)))
              (if (a) ((wood chuck  roast)))
              (could chuck roast wood))
  (insertR* 'roast
            'chuck
            '((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              (could chuck wood))))

(test-end "insertR*-test")

(test-begin "occur*-test")
(test-equal 5
  (occur* 'banana
          '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
(test-end "occur*-test")

(test-begin "subst*-test")
(test-equal '((orange)
              (split ((((orange ice)))
                      (cream (orange))
                      sherbet))
              (orange)
              (bread)
              (orange brandy))
  (subst* 'orange
          'banana
          '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
(test-end "subst*-test")

(test-begin "insertL*-test")
(test-equal '((how much (wood))
              could
              ((a (wood) pecker chuck))
              (((pecker chuck)))
              (if (a) ((wood pecker chuck)))
              (could pecker chuck wood))
  (insertL* 'pecker
            'chuck
            '((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              (could chuck wood))))
(test-end "insertL*-test")

(test-begin "member*-test")
(test-equal #t
  (member* 'chips
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #f
  (member* 'hogehoge
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #t
  (member* 'fish
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #t
  (member* 'much '((how much (wood))
                   could
                   ((a (wood) chuck))
                   (((chuck)))
                   (if (a) ((wood chuck)))
                   (could chuck wood))))
(test-end "member*-test")

(test-begin "leftmost-test")
(test-equal 'potato
  (leftmost '((potato) (chips ((with) fish) (chips)))))
(test-equal 'hot
  (leftmost '(((hot) (tuna (and))) cheese)))
(test-error (leftmost '(((() four)) 17 (seventeen))))
(test-error (leftmost (quote ())))
(test-end "leftmost-test")

(test-begin "and-review-test")
(test-equal #f (and (atom? (car '(mozzarella pizza)))
                    (eq? '(mozzarella pizza) 'pizza)))
(test-equal #f (and (atom? (car '((mozzarella mushuroom) pizza)))
                    (eq? '((mozzarella mushuroom) pizza) 'pizza)))
(test-end "and-review-test")

(test-begin "eqlist?-test")
(test-equal #t
  (eqlist? '(strawberry ice cream)
           '(strawberry ice cream)))
(test-equal #f
  (eqlist? '(strawberry ice cream)
           '(strawberry cream ice)))
(test-equal #f
  (eqlist? '(banana ((split)))
           '((banana) (split))))
(test-equal #f
  (eqlist? '(beef ((sausage)) (and (soda)))
           '(beef ((salami)) (and (soda)))))
(test-equal #t
  (eqlist? '(beef ((sausage)) (and (soda)))
           '(beef ((sausage)) (and (soda)))))
(test-end "eqlist?-test")

(test-begin "equal??-test")
(test-equal #t (equal?? 1 1))
(test-equal #f (equal?? 1 0))
(test-equal #t (equal?? 'banana 'banana))
(test-equal #f (equal?? 'banana 'apple))
(test-equal #t (equal?? '(i like banana) '(i like banana) ))
(test-equal #f (equal?? '(i like banana) '(i love banana) ))
(test-end "equal??-test")

(test-begin "eqlist?2-test")
(test-equal #t
  (eqlist?2 '(strawberry ice cream)
           '(strawberry ice cream)))
(test-equal #f
  (eqlist?2 '(strawberry ice cream)
           '(strawberry cream ice)))
(test-equal #f
  (eqlist?2 '(banana ((split)))
           '((banana) (split))))
(test-equal #f
  (eqlist?2 '(beef ((sausage)) (and (soda)))
           '(beef ((salami)) (and (soda)))))
(test-equal #t
  (eqlist?2 '(beef ((sausage)) (and (soda)))
           '(beef ((sausage)) (and (soda)))))
(test-end "eqlist?2-test")

(test-begin "rember-test")
(test-equal '(apples oranges)
  (rember
   '(foo (bar (baz)))
   '(apples (foo (bar (baz))) oranges)))
(test-end "rember-test")
