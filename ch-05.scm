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

(define insertR*
  (lambda (new old l)
    "insertR*"))

(test-begin "insertR*-test")
(test-equal '((how much (wood))
              could
              ((a (wood)))
              (((chuck)))
              (if (a) ((wood)))
              (could wood))
  (insertR* 'roast
            'chuck
            '((how much (wood))
              could
              ((a (wood) chunk))
              (((chuck)))
              (if (a) ((wood chunk)))
              (could chuck wood))))

(test-end "insertR*-test")
