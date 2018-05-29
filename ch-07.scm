#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

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

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat)
            (makeset (cdr lat)))))))

(test-begin "makeset-test")

(let ((lat '(apple peach pear peach plum apple lemon peach)))
  (test-equal '(pear plum apple lemon peach)
    (makeset lat)))


(test-end "makeset-test")

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
            (makeset
             (multirember (car lat)
                          (cdr lat))))))))

(test-begin "makeset-test2")

(let ((lat '(apple peach pear peach plum apple lemon peach)))
  (test-equal '(apple peach pear plum lemon)
    (makeset lat)))

(let ((lat '(apple 3 pear 4 9 apple 3 4)))
  (test-equal '(apple 3 pear 4 9)
    (makeset lat)))

(test-end "makeset-test2")

;;subset: 部分集合
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

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

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

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

;;intersection: 共通部分

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

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

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

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

;;union: 和集合
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
            (union (cdr set1) set2))))))

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

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
                 (intersectall (cdr l-set)))))))

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

;;pair: 2つのアトムか2つのS式(アトムかS式のリスト(空でもよい)からなる
(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

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

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (a1 a2)
    (cons a1
          (cons a2 '()))))

;;rel: ペアの集合、即ち重複したペアが存在しないペアのリストとの事
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(test-begin "fun?-test")

(test-equal #f
  (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #t
  (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #f
  (fun? '((b 4) (b 0) (b 9) (e 5) (g 4))))

(test-end "fun?-test")

(define repair
  (lambda (pair)
    (build
     (second pair)
     (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (repair (car rel))
            (revrel (cdr rel)))))))

(test-begin "revrel-tets")

(test-equal '((a 8) (pie pumpkin) (sick got))
  (revrel '((8 a) (pumpkin pie) (got sick))))

(test-equal '()
  (revrel '()))

(test-end "revrel-tets")

;;全単射
(define fullfun?
  (lambda (fun)
    #t))

(test-begin "fullfun?-test")

(test-equal #f
  (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(test-equal #t
  (fullfun? '((8 3) (4 8) (4 8) (7 6) (3 4))))

(test-equal #f
  (fullfun? '((grape raisin)
              (plum prune)
              (stewed prune))))

(test-equal #t
  (fullfun? '((grape raisin)
              (plum prune)
              (stewed grape))))

(test-end "fullfun?-test")
