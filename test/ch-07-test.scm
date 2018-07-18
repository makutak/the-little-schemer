#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-07)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "set?-test")

(let ((lat '(apple peaches apple plum)))
  (test-equal #f
    (set? lat)))

(let ((lat '(apples peaches pears plums)))
  (test-equal #t
    (set? lat)))

(let ((lat '()))
  (test-equal #t
    (set? lat)))

(let ((lat '(apple 3 pear 4 9 apple 3 4)))
  (test-equal #f
    (set? lat)))

(test-end "set?-test")

(test-begin "makeset-test")

(let ((lat '(apple peach pear peach plum apple lemon peach)))
  (test-equal '(apple peach pear plum lemon)
    (makeset lat)))

(let ((lat '(apple 3 pear 4 9 apple 3 4)))
  (test-equal '(apple 3 pear 4 9)
    (makeset lat)))

(test-end "makeset-test")

(test-begin "subset?-test")

(let ((set1 '(5 chicken wings))
      (set2 '(5 hamburgers 2 pieces freid chicken and light duckling wings)))
  (test-equal #t
    (subset? set1 set2)))

(let ((set1 '(4 pounds of horseradish))
      (set2 '(four pounds chcken and 5 ounces horseradish)))
  (test-equal #f
    (subset? set1 set2)))
(test-end "subset?-test")

(test-begin "eqset?-test")
(let ((set1 '(6 large chickens with wings))
      (set2 '(6 chickens with large wings)))
  (test-equal #t
    (eqset? set1 set2)))

(let ((set1 '(6 large chckens with wings hoge))
      (set2 '(6 chickens with large wings)))
  (test-equal #f
    (eqset? set1 set2)))

(test-end "eqset?-test")

(test-begin "intersect?-test")

(let ((set1 '(stewed tomatoes and macaroni))
      (set2 '(macaroni and cheese)))
  (test-equal #t
    (intersect? set1 set2)))

(let ((set1 '(stewed tomatoes or macaronis))
      (set2 '(macaroni and cheese)))
  (test-equal #f
    (intersect? set1 set2)))

(test-end "intersect?-test")

(test-begin "intersect-test")

(let ((set1 '(stewed tomatoes and macaroni))
      (set2 '(macaroni and cheese)))
  (test-equal '(and macaroni)
    (intersect set1 set2)))

(let ((set1 '(stewed tomatoes or macaronis))
      (set2 '(macaroni and cheese)))
  (test-equal '()
    (intersect set1 set2)))

(test-end "intersect-test")

(test-begin "union-test")

(let ((set1 '(stewed tomatoes and macaroni casserole))
      (set2 '(macaroni and cheese)))
  (test-equal '(stewed tomatoes casserole macaroni and cheese)
    (union set1 set2)))

(let ((set1 '())
      (set2 '(macaroni and cheese)))
  (test-equal '(macaroni and cheese)
    (union set1 set2)))

(test-end "union-test")

(test-begin "intersectall-test")

(test-equal '(a)
  (intersectall '((a b c)
                  (c a d e)
                  (e f g h a b))))

(test-equal '(6 and)
  (intersectall '((6 pears and)
                  (3 peaches and 6 peppers)
                  (8 pears and 6 plums)
                  (and 6 prunes with lots of apples))))

(test-end "intersectall-test")

(test-begin "a-pair?-test")

(test-equal #t
  (a-pair? '(3 7)))

(test-equal #t
  (a-pair? '((2) (pair))))

(test-equal #t
  (a-pair? '(full (hourse))))

(test-equal #f
  (a-pair? 'hoge))

(test-equal #f
  (a-pair? '(1 2 3)))

(test-equal #f
  (a-pair? '(1 '(foo) 'bar)))

(test-end "a-pair?-test")

(test-begin "fun?-test")

(test-equal #f
  (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #t
  (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #f
  (fun? '((b 4) (b 0) (b 9) (e 5) (g 4))))

(test-end "fun?-test")

(test-begin "revrel-test")

(test-equal '((a 8) (pie pumpkin) (sick got))
  (revrel '((8 a) (pumpkin pie) (got sick))))

(test-equal '()
  (revrel '()))

(test-end "revrel-test")

(test-begin "fullfun?-test")

(test-equal #f
  (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #t
  (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))

(test-equal #f
  (fullfun? '((grape raisin)
              (plum prune)
              (stewed prune))))

(test-equal #t
  (fullfun? '((grape raisin)
              (plum prune)
              (stewed grape))))

(test-end "fullfun?-test")

(test-begin "one-to-one?-test")

(test-equal #f
  (one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #t
  (one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4))))

(test-equal #f
  (one-to-one? '((grape raisin)
                 (plum prune)
                 (stewed prune))))

(test-equal #t
  (one-to-one? '((grape raisin)
                 (plum prune)
                 (stewed grape))))

(test-equal #t
  (one-to-one? '((chocolate chip) (doughy cookie))))

(test-end "one-to-one?-test")
