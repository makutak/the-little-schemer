#!/usr/local/bin/guile
!#

(use-modules (srfi srfi-64))
(load "lib/util.scm")

(test-begin "ch-02-test")

(test-eqv #t (lat? '(Jack Sprat eat no chicken fat)))
(test-eqv #f (lat? '((Jack) Sprat eat not chicken fat)))
(test-eqv #f (lat? '((Jack) sprat eat not chicken fat)))
(test-eqv #t (lat? '()))

(test-eqv #t (lat? '(bacon and eggs)))
;; scheme@(guile-user)> ,trace (lat? '(bacon and eggs))
;; trace: (lat? (bacon and eggs))
;; trace: |  (atom? bacon)
;; trace: |  #t
;; trace: (lat? (and eggs))
;; trace: |  (atom? and)
;; trace: |  #t
;; trace: (lat? (eggs))
;; trace: |  (atom? eggs)
;; trace: |  #t
;; trace: (lat? ())
;; trace: #t

(test-eqv #f (lat? '(bacon (and eggs))))
;; scheme@(guile-user)> ,trace (lat? '(bacon (and eggs)))
;; trace: (lat? (bacon (and eggs)))
;; trace: |  (atom? bacon)
;; trace: |  #t
;; trace: (lat? ((and eggs)))
;; trace: |  (atom? (and eggs))
;; trace: |  #f
;; trace: #f

(test-eqv #t (or (null? '()) (atom? '(d f g))))
(test-eqv #t (or (null? '(a b c)) (null? '())))
(test-eqv #f (or (null? '(a b c)) (null? '(atom))))

(test-eqv #t (member? 'tea '(coffee tea or milk)))
(test-eqv #f (member? 'poached '(fired eggs and scrambled eggs)))
(test-eqv #t (member? 'meat '(mashed potatoes and meat gravy)))
;; scheme@(guile-user)> ,trace (member? 'meat '(mashed potatoes and meat gravy))
;; trace: (member? meat (mashed potatoes and meat gravy))
;; trace: (member? meat (potatoes and meat gravy))
;; trace: (member? meat (and meat gravy))
;; trace: (member? meat (meat gravy))
;; trace: #t
(test-eqv #f (member? 'liver '(bageles and lox)))
;; scheme@(guile-user)> ,trace (member? 'liver '(bageles and lox))
;; trace: (member? liver (bageles and lox))
;; trace: (member? liver (and lox))
;; trace: (member? liver (lox))
;; trace: (member? liver ())
;; trace: #f

(test-end)
