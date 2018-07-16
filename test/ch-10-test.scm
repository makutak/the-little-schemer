#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-10)
             (srfi srfi-64))

(set! test-log-to-file #f)

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

(test-begin "meaning-test")

(test-equal '(non-primitive ((((y z) ((8) 9))) (x) (cons x y)))
  (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9)))))

(test-end "meaning-test")

(test-begin "value-test")

(test-equal 11
  (value '(add1 10)))

(test-equal 334
  (value '(sub1 335)))

(test-equal #t
  (value '(null? '())))

(test-equal #f
  (value '(null? '(foo))))

(test-equal #t
  (value '(number? 1)))

(test-equal #f
  (value '(number? 'foo)))

(test-equal 'foo
  (value '(car '(foo bar))))

(test-equal '(bar)
  (value '(cdr '(foo bar))))

(test-equal '(foo hoge fuga)
  (value '(cons 'foo '(hoge fuga))))

(test-equal '((foo bar baz))
  (value
   '((lambda (x)
       (cons x (quote ())))
     (quote (foo bar baz)))))

(test-equal 'true
            (value '((lambda (x)
                       (cond
                        (x (quote true))
                        (else
                         (quote false))))
                     #t)))

(test-end "value-test")
