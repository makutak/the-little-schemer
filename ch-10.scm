#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

;;entry: 連想配列のようなもの

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help name
                            (cdr names)
                            (cdr values)
                            entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(test-begin "lookup-in-entry-test")

(test-equal 'tastes
  (lookup-in-entry 'entree
                   '((appetier entree beverage) ;; keys
                     (food tastes good))        ;; values
                   (lambda (x) '())))

(test-equal '()
  (lookup-in-entry 'desert
                   '((appetier entree beverage) ;; keys
                     (food tastes good))        ;; values
                   (lambda (x) '())))

(test-equal 'nothing!!
  (lookup-in-entry 'desert
                   '((appetier entree beverage) ;; keys
                     (food tastes good))        ;; values
                   (lambda (x) 'nothing!!)))

(test-end "lookup-in-entry-test")


;;table: entryのリスト

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
                       (car table)
                       (lambda (name)
                         (lookup-in-table name
                                          (cdr table)
                                          table-f)))))))

(test-begin "lookup-in-table-test")

(test-equal 'spaghetti
  (lookup-in-table 'entree
                  '(((entree desert)
                     (spaghetti spumoni))
                    ((appetizer entree beverage)
                     (food tastes good)))
                  (lambda (name) '())))

(test-equal 'good
  (lookup-in-table 'beverage
                  '(((entree desert)
                     (spaghetti spumoni))
                    ((appetizer entree beverage)
                     (food tastes good)))
                  (lambda (name) '())))

(test-equal '()
  (lookup-in-table 'foo
                  '(((entree desert)
                     (spaghetti spumoni))
                    ((appetizer entree beverage)
                     (food tastes good)))
                  (lambda (name) '())))

(test-end "lookup-in-table-test")

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookip-in-table e table initial-table)))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define *lambda
  (lambda (e table)
    (build (quote no-primitive)
           (cons table (cdr e)))))

(define *cond '*cond)

(define *application '*application)

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t)  *const)
     ((eq? e #f)  *const)
     ((eq? e (quote cons))  *const)
     ((eq? e (quote car))   *const)
     ((eq? e (quote cdr))   *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?))   *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1))  *const)
     ((eq? e (quote sub1))  *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote))
        *quote)
       ((eq? (car e) (quote lambda))
        *lambda)
       ((eq? (car e) (quote cond))
        *cond)
       (else *application)))
     (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e)
      (atom-to-action e))
     (else
      (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(test-begin "meaning-test")

(test-equal '(no-primitive ((((y z) ((8) 9))) (x) (cons x y)))
  (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9)))))

(test-end "meaning-test")

(define value
  (lambda (e)
    (meaning e '())))
