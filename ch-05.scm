#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else
        (cons
         (car l)
         (rember* a (cdr l))))))
     (else
      (cons
       (rember* a (car l))
       (rember* a (cdr l)))))))

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
