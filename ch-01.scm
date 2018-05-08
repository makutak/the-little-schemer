(use-modules (srfi srfi-64))
(load "lib/util.scm")

(test-begin "ch-01-test")

;;これはアトムですか?
(test-assert (atom? 'atom))
(test-assert (atom? (quote atom)))
(test-assert (atom? 'turkey))
(test-assert (atom? (quote turkey)))
(test-assert (atom? '1492))
(test-assert (atom? (quote 1492)))
(test-assert (atom? 'u))
(test-assert (atom? (quote u)))

;;これはリストですか?
(test-assert (list? '(atom)))
(test-assert (list? '(atom tukey or)))
(test-error  (list? '(atom turkey) 'or))
(test-assert (list? '((atom turkey) or)))

;;これはS式ですか?
(test-assert (not (null? 'xyz)))
(test-assert (not (null? '(x y z))))
(test-assert (not (null? '((x y) z))))

;;複合
(test-assert (list? '(how are you doing so far)))
(test-assert (eq? (length '(how are you doing so far)) 6))
(test-assert (list? '(((how) are) '('(you) '(doing so)) far)))
(test-assert (eq? (length '(((how) are) '((you) '(doing so)) far)) 3))
(test-assert (list? '()))
(test-assert (not (atom? '())))
(test-assert (list? '('() '() '())))

;;car
(test-assert (eq? (car '(a b c)) 'a))
(test-assert (eq? (car '((a b c) x y z)) '(a b c)))
(test-assert (eq? (car '(((hotdogs)) '(and) '(pickle) relish)) '((hotdogs))))
(test-assert (eq? (car (car '(((hotdogs)) '(and)))) '(hotdogs)))

;;cdr
(test-assert (eq? (cdr '(a b c)) '(b c)))
(test-assert (eq? (cdr '((a b c) x y z)) '(x y z)))
(test-assert (eq? (cdr '(hamburger)) '()))
(test-assert (eq? (cdr '((x) t r))) '(t r))
(test-error (cdr 'hotdogs))
(test-error (cdr '()))

;;car and cdr
(test-assert (eq? (car (cdr '((b) (x y) ((c)))))) '(x y))
(test-assert (eq? (cdr (cdr '((b) (x y) ((c)))))) '((c)))
(test-error (cdr (car '(a (b (c)) d))))

;;cons
(test-assert (equal? (cons 'peanut '(butter and jerry)) '(peanut butter and jerry)))
(test-assert (equal? (cons '(banana and) '(peanut butter and jerry))) '((banana and) peanut butter and jerry))
(test-assert (equal? (cons '((help) this) '(is very ((hard) to learn)))) '(((help) this) is very ((hard) to learn)))
(test-assert (equal? (cons '(a b (c)) '())) '(a b (c)))
(test-assert (equal? (cons 'a '()) '(a)))
(test-assert (not (list? (cons '((a b c)) 'b))))
(test-assert (not (list? (cons 'a 'b))))

;;car, cdr, cons
(test-assert (equal? (cons 'a (car '((b) c d))) '(a b)))
(test-assert (equal? (cons 'a (cdr '((b) c d))) '(a c d)))
(test-assert (null? '()))
(test-assert (eq? #t (null? (quote ()))))
(test-assert (not (null? '(a b c))))
(test-assert (not (null? 'spaghetti)))

;;null?
(test-assert (atom? 's))
(test-assert (atom? 'Harry))
(test-assert (not (atom? '(Harry had a heap of apples))))
(test-assert (atom? (car '(Harry had a heap of apples))))
(test-assert (not (atom? (cdr '(Harry had a heap of apples)))))
(test-assert (not (atom? '(Harry))))
(test-assert (atom? (car (cdr '(swing low sweet cherry oat)))))
(test-assert (not (atom? (car (cdr '(swing (low sweet) cherry oat))))))
(test-assert (eq? 'Harry 'Harry))
(test-assert (not (eq? 'margarine 'butter)))
(test-assert (not (eq? '() '(strawberry))))
(test-assert (not (eq? 6 7)))
(test-assert (eq? (car '(Marry had a little lamb chop))) 'Marry)
(test-assert (not (eq? (cdr '(soured milk)) 'milk)))
(test-assert (eq? (car '(beans beans we need jerry beand))) (car (cdr '(beans beans we need jerry beand))))

(test-end "toys-test")
