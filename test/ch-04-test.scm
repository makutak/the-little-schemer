#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(add-to-load-path "..")

(use-modules (ch-04)
             (lib util)
             (srfi srfi-64))

(set! test-log-to-file #f)

(test-begin "add1-test")
(test-equal 68 (add1 67))
(test-end "add1-test")

(test-begin "sub1-test")
(test-equal 4 (sub1 5))
(test-end "sub1-test")

(test-begin "zero?-test")
(test-equal #t (zero? 0))
(test-equal #f (zero? 1492))
(test-end "zero?-test")

(test-begin "o+-test")
(test-equal 58 (o+ 46 12))
(test-end "o+-test")

(test-begin "o--test")
(test-equal 11 (o- 14 3))
(test-equal 8 (o- 17 9))
(test-end "o--test")

(test-begin "addtup-test")
(test-equal 18 (addtup '(3 5 2 8)))
(test-equal 43 (addtup '(15 6 7 12 3)))
(test-end "addtup-test")

(test-begin "o*-test")
(test-equal 36 (o* 12 3))
(test-end "o*-test")

(test-begin "tup+-test")
(test-equal '(11 11 11 11 11) (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
(test-equal '(6 9) (tup+ '(2 3) '(4 6)))
(test-equal '(7 13) (tup+ '(3 7) '(4 6)))
(test-equal '(7 13 8 1) (tup+ '(3 7) '(4 6 8 1)))
(test-end "tup+-test")

(test-begin "o>-test")
(test-equal #f (o> 12 133))
(test-equal #t (o> 120 11))
(test-equal #f (o> 3 3))
(test-end "o>-test")

(test-begin "o<-test")
(test-equal #t (o< 4 6))
(test-equal #f (o< 8 3))
(test-equal #f (o< 6 6))
(test-end "o<-test")

(test-begin "o=-test")
(test-equal #t (o= 1 1))
(test-equal #f (o= 1 0))
(test-equal #f (o= 0 1))
(test-end "o=-test")

(test-begin "o^-test")
(test-equal 1 (o^ 1 1))
(test-equal 8 (o^ 2 3))
(test-equal 125 (o^ 5 3))
(test-equal 1 (o^ 100000 0))
(test-end "o^-test")

(test-begin "o/-test")
(test-equal 3 (o/ 15 4))
(test-equal 20 (o/ 100 5))
(test-end "o/-test")

(test-begin "o-length-test")
(test-equal 6
  (o-length '(hotdogs with mustard sauerkraut and pickles)))
(test-equal 5
  (o-length '(ham and cheese on rye)))
(test-equal 0
  (o-length '()))
(test-end "o-length-test")

(test-begin "pick-test")
(test-equal 'macaroni
  (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))
(test-error (pick 0 '(a)))
(test-end "pick-test")

(test-begin "no-nums-test")
(test-equal '(pears prunes dates) (no-nums '(5 pears 6 prunes 9 dates)))
(test-end "no-nums-test")

(test-begin "all-nums-test")
(test-equal '(5 6 9) (all-nums '(5 pears 6 prunes 9 dates)))
(test-end "all-nums-test")

(test-begin "eqan?-test")
(test-equal #t (eqan? 1 1))
(test-equal #f (eqan? 1 2))
(test-equal #f (eqan? 'a 1))
(test-equal #t (eqan? 'a 'a))
(test-equal #f (eqan? 'a 'b))
(test-end "eqan?-test")

(test-begin "occur-test")
(test-equal 2 (occur 'big '(a big black bug bit a big black bear)))
(test-equal 1 (occur 'bit '(a big black bug bit a big black bear)))
(test-equal 0 (occur 'hoge '(a big black bug bit a big black bear)))
(test-equal 2 (occur 'black '(a big black bug bit a big black bear)))
(test-end "occur-test")

(test-begin "one?-test")
(test-equal #t (one? 1))
(test-equal #f (one? 2039))
(test-equal #f (one? 0))
(test-end "one?-test")

(test-begin "rempick-test")
(test-equal '(hotdogs with mustard)
  (rempick 3 '(hotdogs with hot mustard)))
(test-equal '(lemon meringue pie)
  (rempick 3 '(lemon meringue salty pie)))
(test-end"rempick-test")
