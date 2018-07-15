#!/usr/local/bin/guile
!#

(add-to-load-path (dirname (current-filename)))

(define-module (ch-10)
  #:use-module (lib util)
  #:export (lookup-in-entry
            lookup-in-table
            meaning))

;;entry: 連想配列のようなもの

(define new-entry build)

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
;;table: entryのリスト

(define extend-table cons)

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

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e)
      (atom-to-action e))
     (else
      (list-to-action e)))))

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

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define value
  (lambda (e)
    (meaning e (quote ()))))

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
    (lookup-in-table e table initial-table)))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define *lambda
  (lambda (e table)
    (build (quote no-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define *application '*application)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e)
           table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
               table))
     ((meaning (question-of (car lines))
               table)
      (meaning (answer-of (car lines))
               table))
     (else
      (evcon (cdr lines)
             table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x)
      (eq? x (quote else)))
     (else #f))))

(define question-of first)

(define answer-of second)
