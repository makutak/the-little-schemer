#!/usr/local/bin/guile
!#

(add-to-load-path "./lib/")
(import (srfi srfi-64) (util))

(set! test-log-to-file #f)

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else
        (cons
         (car l)
         (rember* a (cdr l))))))
     (else
      (cons
       (rember* a (car l))
       (rember* a (cdr l)))))))

(test-begin "rember*-test")
(test-equal '((coffee) ((tea))
              (and (hick)))
  (rember* 'cup
           '((coffee) cup ((tea) cup)
             (and (hick)) cup)))

(test-equal '(((tomato))
              ((bean))
              (and ((flying))))
  (rember* 'sauce '(((tomato sauce))
                   ((bean) sauce)
                   (and ((flying)) sauce))))
(test-end "rember*-test")

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old (cdr l)))))
       (else
        (cons
         (car l)
         (insertR* new old (cdr l))))))
     (else
      (cons
       (insertR* new old (car l))
       (insertR* new old (cdr l)))))))

(test-begin "insertR*-test")
(test-equal '((how much (wood))
              could
              ((a (wood) chuck roast))
              (((chuck roast)))
              (if (a) ((wood chuck  roast)))
              (could chuck roast wood))
  (insertR* 'roast
            'chuck
            '((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              (could chuck wood))))

(test-end "insertR*-test")

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else
        (occur* a (cdr l)))))
     (else
      (o+
       (occur* a (car l))
       (occur* a (cdr l)))))))

(test-begin "occur*-test")
(test-equal 5
  (occur* 'banana
          '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
(test-end "occur*-test")

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons
       (subst* new old (car l))
       (subst* new old (cdr l)))))))

(test-begin "subst*-test")
(test-equal '((orange)
              (split ((((orange ice)))
                      (cream (orange))
                      sherbet))
              (orange)
              (bread)
              (orange brandy))
  (subst* 'orange
          'banana
          '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
(test-end "subst*-test")

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old (cdr l)))))
       (else
        (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons
       (insertL* new old (car l))
       (insertL* new old (cdr l)))))))

(test-begin "insertL*-test")
(test-equal '((how much (wood))
              could
              ((a (wood) pecker chuck))
              (((pecker chuck)))
              (if (a) ((wood pecker chuck)))
              (could pecker chuck wood))
  (insertL* 'pecker
            'chuck
            '((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              (could chuck wood))))
(test-end "insertL*-test")

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or
       (eq? (car l) a)
       (member* a (cdr l))))
     (else
      (or
       (member* a (car l))
       (member* a (cdr l)))))))

(test-begin "member*-test")
(test-equal #t
  (member* 'chips
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #f
  (member* 'hogehoge
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #t
  (member* 'fish
           '((potato)
             (chips
              ((with) fish) (chips)))))
(test-equal #t
  (member* 'much '((how much (wood))
                   could
                   ((a (wood) chuck))
                   (((chuck)))
                   (if (a) ((wood chuck)))
                   (could chuck wood))))
(test-end "member*-test")

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

(test-begin "leftmost-test")
(test-equal 'potato
  (leftmost '((potato) (chips ((with) fish) (chips)))))
(test-equal 'hot
  (leftmost '(((hot) (tuna (and))) cheese)))
(test-error (leftmost '(((() four)) 17 (seventeen))))
(test-error (leftmost (quote ())))
(test-end "leftmost-test")

(test-begin "and-review-test")
(test-equal #f (and (atom? (car '(mozzarella pizza)))
                    (eq? '(mozzarella pizza) 'pizza)))
(test-equal #f (and (atom? (car '((mozzarella mushuroom) pizza)))
                    (eq? '((mozzarella mushuroom) pizza) 'pizza)))
(test-end "and-review-test")

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2))
      #t)
     ((or (null? l1) (null? l2))
      #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2)))
      #f)
     (else
      (and
       (eqlist? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2)))))))

(test-begin "eqlist?-test")
(test-equal #t
  (eqlist? '(strawberry ice cream)
           '(strawberry ice cream)))
(test-equal #f
  (eqlist? '(strawberry ice cream)
           '(strawberry cream ice)))
(test-equal #f
  (eqlist? '(banana ((split)))
           '((banana) (split))))
(test-equal #f
  (eqlist? '(beef ((sausage)) (and (soda)))
           '(beef ((salami)) (and (soda)))))
(test-equal #t
  (eqlist? '(beef ((sausage)) (and (soda)))
           '(beef ((sausage)) (and (soda)))))
(test-end "eqlist?-test")

(define equal??
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else
      (eqlist? s1 s2)))))

(test-begin "equal??-test")
(test-equal #t (equal?? 1 1))
(test-equal #f (equal?? 1 0))
(test-equal #t (equal?? 'banana 'banana))
(test-equal #f (equal?? 'banana 'apple))
(test-equal #t (equal?? '(i like banana) '(i like banana) ))
(test-equal #f (equal?? '(i like banana) '(i love banana) ))
(test-end "equal??-test")

(define eqlist?2
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2))
      #t)
     ((or (null? l1) (null? l2))
      #f)
     (else
      (and
       (equal?? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2)))))))

(test-begin "eqlist?2-test")
(test-equal #t
  (eqlist?2 '(strawberry ice cream)
           '(strawberry ice cream)))
(test-equal #f
  (eqlist?2 '(strawberry ice cream)
           '(strawberry cream ice)))
(test-equal #f
  (eqlist?2 '(banana ((split)))
           '((banana) (split))))
(test-equal #f
  (eqlist?2 '(beef ((sausage)) (and (soda)))
           '(beef ((salami)) (and (soda)))))
(test-equal #t
  (eqlist?2 '(beef ((sausage)) (and (soda)))
           '(beef ((sausage)) (and (soda)))))
(test-end "eqlist?2-test")

(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal?? (car l) s)
      (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))

(test-begin "rember-test")
(test-equal '(apples oranges)
  (rember
   '(foo (bar (baz)))
   '(apples (foo (bar (baz))) oranges)))
(test-end "rember-test")
