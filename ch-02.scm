(use-modules (srfi srfi-64))
(load "lib/util.scm")

(test-begin "ch-02-test")

(test-eqv #t (lat? '(Jack Sprat eat no chicken fat)))
(test-eqv #f (lat? '((Jack) Sprat eat not chicken fat)))
(test-eqv #f (lat? '((Jack) sprat eat not chicken fat)))
(test-eqv #t (lat? '()))


(test-end)
