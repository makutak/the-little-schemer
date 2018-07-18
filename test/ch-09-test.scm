#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-09)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "keep-looking-test")

(test-equal #t
  (keep-looking 'caviar
                3
                '(6 2 4 caviar 5 7 3)))

(test-equal #t
  (keep-looking 'caviar
                4
                '(6 2 4 caviar 5 7 3)))

(test-end "keep-looking-test")

(test-begin "looking-test")

(test-equal #t
  (looking 'caviar
           '(6 2 4 caviar 5 7 3)))

(test-equal #f
  (looking 'caviar
           '(6 2 grits caviar 5 7 3)))

(test-equal #t
  (looking 'caviar
           '(6 2 4 caviar 5 7 3)))

(test-end "looking-test")

(test-begin "shift-test")

(test-equal '(a (b c))
  (shift '((a b) c)))

(test-equal '(a (b (c d)))
  (shift '((a b) (c d))))

(test-end "shift-test")
(test-begin "weight*-test")

(test-equal 7
  (weight* '((a b) c)))

(test-equal 5
  (weight* '(a (b c))))

(test-end "weight*-test")

(test-begin "shuffle-test")

(test-equal '(a (b c))
  (shuffle '(a (b c))))

(test-equal '(a b)
  (shuffle '(a b)))

(test-end "shuffle-test")


(test-begin "A-test")

(test-equal 2
  (A 1 0))

(test-equal 3
  (A 1 1))

(test-equal 7
  (A 2 2))

(test-end "A-test")


(test-begin "Y-combinator-test")

(test-equal 0
  ((Y (lambda (length)
         (lambda (l)
           (cond
            ((null? l) 0)
            (else (add1 (length (cdr l))))))))
   '()))

(test-equal 5
  ((Y (lambda (length)
         (lambda (l)
           (cond
            ((null? l) 0)
            (else (add1 (length (cdr l))))))))
   '(1 2 3 4 5)))

(test-equal 7
  ((Y (lambda (length)
         (lambda (l)
           (cond
            ((null? l) 0)
            (else (add1 (length (cdr l))))))))
   '(1 2 3 4 5 6 7)))

(test-end "Y-combinator-test")
