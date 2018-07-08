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
