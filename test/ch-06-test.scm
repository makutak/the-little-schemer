#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-06)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)


(test-begin "numbered?-test")

(let ((x 1))
  (test-equal #t
    (numbered? x)))

(let ((x '(1 o+ 1)))
  (test-equal #t
    (numbered? x)))

(let ((y '(3 o+ (4 o^ 5))))
  (test-equal #t
    (numbered? y)))

(let ((y '(2 o* sausage)))
  (test-equal #f
    (numbered? y)))

(test-end "numbered?-test")

(test-begin "value-test")

(let ((u 13))
  (test-equal 13
    (value 13)))

(let ((x '(1 o+ 3)))
  (test-equal 4
    (value x)))

(let ((y '(1 o+ (3 o^ 4))))
  (test-equal 82
    (value y)))

(let ((z 'cookie))
  (test-equal 'cookie
   (value z)))

(let ((a '(1 o+ (3 o* 4))))
  (test-equal 13
    (value a)))

(let ((b '((1 o+ 3) o* 4)))
  (test-equal 16
    (value b)))

(test-end "value-test")

(test-begin "value-prefix-test")

(let ((x '(o+ 3 4)))
  (test-equal 7
    (value-prefix x)))

(let ((y '(o+ (o* 3 6) (o^ 8 2))))
  (test-equal 82
    (value-prefix y)))

(test-end "value-prefix-test")

(test-begin "sero?-test")
(test-equal #t (sero? '()))
(test-equal #f (sero? '(())))
(test-end "sero?-test")

(test-begin "edd1-test")

(test-equal '(())
  (edd1 '()))

(test-equal '(() () ())
  (edd1 '(() ())))

(test-equal '(() ())
  (edd1 '(())))

(test-end "edd1-test")


(test-begin "zub1-test")

(test-error (zub1 '()))

(test-equal '(())
  (zub1 '(() ())))

(test-equal '(() ())
  (zub1 '(() () ())))

(test-end "zub1-test")

(test-begin ".+-test")

(test-equal '(() ())
  (.+ '(()) '(())))

(test-equal '(() () ())
  (.+ '(()) '(() ())))

(test-end ".+-test")

(test-begin "new-number-lat?-test")
(let ((ls '((()) (() ()) (() () ()))))
  (test-equal #f
    (lat? ls)))
(test-end "new-number-lat?-test")
