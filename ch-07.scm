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

(let ((set1 '(5 chcken wings))
      (set2 '(5 hamburgers 2 pieces freid chicken and light duckling wings)))
  (test-equal #t
    (subset? set1 set2)))

(let ((set1 '(4 pounds of horseradish))
      (set2 '(four pounds chcken and 5 ounces horseradish)))
  (test-equal #f
    (subset? set1 set2)))
(test-end "subset?-test")