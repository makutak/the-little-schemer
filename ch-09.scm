#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(define-module (ch-09)
  #:use-module (lib util)
  #:use-module (srfi srfi-64)
  #:export (keep-looking
            looking
            shift
            weight*
            shuffle
            A
            Y))

(set! test-log-to-file #f)

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (para)
    (cond
     ((atom? para)
      para)
     ((a-pair? (first para))
      (align (shift para)))
     (else
      (build (first para)
             (align (second para)))))))
;;=> cond でshiftはalignのために引数を生成するが、それは元の引数の一部分ではない。

(define length*
  (lambda (para)
    (cond
     ((atom? para) 1)
     (else
      (o+ (length* (first para))
          (length* (second para)))))))

(define weight*
  (lambda (para)
    (cond
     ((atom? para) 1)
     (else
      (o+ (o* (weight* (first para)) 2)
          (weight* (second para)))))))


(define shuffle
  (lambda (para)
    (cond
     ((atom? para)
      para)
     ((a-pair? (first para))
      (shuffle (revpair para)))
     (else
      (build (first para)
             (shuffle (second para)))))))


;; (shuffle '((a b) (c d)))
;; => (revpair '((a b) (c d)))
;; ==> '((c d) (a b))
;; となり、循環する?
;; 以下textより、
;; ```
;; この値を決めるためには、(shuffle (revpair para)) が何かを見つけなければいけない
;; (revpair para) が ((a b) (c d)) のときの (shuffle (revpair para)) の値を知る必要がある。
;;```

(define C
  (lambda (n)
    (cond
     ((one? n)
      1)
     (else
      (cond
       ((even?? n)
        (C (o/ n 2)))
       (else
        (C (add1 (o* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n)
      (add1 m))
     ((zero? m)
      (A (sub1 n) 1))
     (else
      (A (sub1 n)
         (A n (sub1 m)))))))

(define my-length
  (lambda (l)
    (cond
     ((null? l)
      0)
     (else
      (add1 (my-length (cdr l)))))))

;;長さ0のリストを求めることができる
(lambda (l)
  (cond
   ((null? l)
    0)
   (else
    (add1 (eternity (cdr l))))))

(test-begin "evaluate-0-length-list-test")

(test-equal 0
  ((lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1 (eternity (cdr l)))))) '()))

(test-end "evaluate-0-length-list-test")

(define length_0
  (lambda (l)
    (cond
     ((null? l)
      0)
     (else
      (add1 (eternity (cdr l)))))))

(lambda (l)
  (cond
   ((null? l)
    0)
   (else
    (add1 (length_0 (cdr l))))))

(test-begin "evaluate-1-length-list-test")

(test-equal 0
  ((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (length_0 (cdr l))))))
   '()))

(test-equal 1
  ((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (length_0 (cdr l))))))
   '(foo)))

(test-end "evaluate-1-length-list-test")

;;長さ1以下のリストの長さを求めることができる
(lambda (l)
  (cond
   ((null? l)
    0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (eternity (cdr l))))))
      (cdr l))))))

(test-begin "evaluate-1-length-list-test-2")

(test-equal 0
  ((lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1
         ((lambda (l)
            (cond
             ((null? l)
              0)
             (else
              (add1 (eternity (cdr l))))))
          (cdr l))))))
   '()))

(test-equal 1
  ((lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1
         ((lambda (l)
            (cond
             ((null? l)
              0)
             (else
              (add1 (eternity (cdr l))))))
          (cdr l))))))
   '(foo)))


(test-end "evaluate-1-length-list-test-2")

;;長さ2以下のリストの長さを求めることができる
(lambda (l)
  (cond
   ((null? l)
    0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1
           ((lambda (l)
              (cond
               ((null? l)
                0)
               (else
                (add1 (eternity (cdr l))))))
            (cdr l))))))
      (cdr l))))))


(test-begin "evaluate-2-length-list-test")

(test-equal 0
  ((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1
        ((lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1
              ((lambda (l)
                 (cond
                  ((null? l)
                   0)
                  (else
                   (add1 (eternity (cdr l))))))
               (cdr l))))))
         (cdr l))))))
   '()))

(test-equal 1
  ((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1
        ((lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1
              ((lambda (l)
                 (cond
                  ((null? l)
                   0)
                  (else
                   (add1 (eternity (cdr l))))))
               (cdr l))))))
         (cdr l))))))
   '(foo)))

(test-equal 2
  ((lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1
        ((lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1
              ((lambda (l)
                 (cond
                  ((null? l)
                   0)
                  (else
                   (add1 (eternity (cdr l))))))
               (cdr l))))))
         (cdr l))))))
   '(foo bar)))


(test-end "evaluate-2-length-list-test")

;;length_0を生成する
((lambda (len)
    (lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1 (len (cdr l)))))))
 eternity)

(test-begin "rewrite-length-0-test")

(test-equal 0
  (((lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l)))))))
    eternity)
   '()))

(test-end "rewrite-length-0-test")

((lambda (f)
    (lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1 (f (cdr l)))))))
 ((lambda (g)
     (lambda (l)
       (cond
        ((null? l)
         0)
        (else
         (add1 (g (cdr l)))))))
  eternity))

(test-begin "rewriter-length_<=1-test")

(test-equal 0
  (((lambda (f)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (f (cdr l)))))))
     ((lambda (g)
        (lambda (l)
          (cond
           ((null? l)
            0)
           (else
            (add1 (g (cdr l)))))))
      eternity)) '()))

(test-equal 1
  (((lambda (f)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (f (cdr l)))))))
     ((lambda (g)
        (lambda (l)
          (cond
           ((null? l)
            0)
           (else
            (add1 (g (cdr l)))))))
      eternity)) '(foo)))

(test-end "rewriter-length_<=1-test")

((lambda (len)
    (lambda (l)
      (cond
       ((null? l)
        0)
       (else
        (add1 (len (cdr l)))))))
 ((lambda (len)
     (lambda (l)
       (cond
        ((null? l)
         0)
        (else
         (add1 (len (cdr l)))))))
  ((lambda (len)
     (lambda (l)
       (cond
        ((null? l)
         0)
        (else
         (add1 (len (cdr l)))))))
   eternity)))

(test-begin "rewrite-length_<=2-test")

(test-equal 0
  (((lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l)))))))
     ((lambda (len)
        (lambda (l)
          (cond
           ((null? l)
            0)
           (else
            (add1 (len (cdr l)))))))
      ((lambda (len)
         (lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1 (len (cdr l)))))))
       eternity)))
   '()))

(test-equal 1
  (((lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l)))))))
     ((lambda (len)
        (lambda (l)
          (cond
           ((null? l)
            0)
           (else
            (add1 (len (cdr l)))))))
      ((lambda (len)
         (lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1 (len (cdr l)))))))
       eternity)))
   '(foo)))

(test-equal 2
  (((lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l)))))))
     ((lambda (len)
        (lambda (l)
          (cond
           ((null? l)
            0)
           (else
            (add1 (len (cdr l)))))))
      ((lambda (len)
         (lambda (l)
           (cond
            ((null? l)
             0)
            (else
             (add1 (len (cdr l)))))))
       eternity)))
   '(foo bar)))

(test-end "rewrite-length_<=2-test")

;;length_0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (len)
   (lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (len (cdr l))))))))

(test-begin "mk-length_0-test")

(test-equal 0
  (((lambda (mk-length)
      (mk-length eternity))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '()))

(test-end "mk-length_0-test")

;;length_<=1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (len)
   (lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (len (cdr l))))))))

(test-begin "mk-length_<=1-test")

(test-equal 0
  (((lambda (mk-length)
       (mk-length
        (mk-length eternity)))
     (lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l))))))))
   '()))

(test-equal 1
  (((lambda (mk-length)
       (mk-length
        (mk-length eternity)))
     (lambda (len)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1 (len (cdr l))))))))
   '(foo)))

(test-end "mk-length_<=1-test")

;;length_<=2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (len)
   (lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (len (cdr l))))))))

(test-begin "mk-length_<=2-test")

(test-equal 0
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length eternity))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '()))

(test-equal 1
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length eternity))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '(foo)))

(test-equal 2
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length eternity))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '(foo bar)))

(test-end "mk-length_<=2-test")

;;length_<=3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (len)
   (lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1 (len (cdr l))))))))

(test-begin "mk-length_<=3-test")

(test-equal 0
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length
         (mk-length eternity)))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '()))

(test-equal 1
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length
         (mk-length eternity)))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '(foo)))

(test-equal 2
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length
         (mk-length eternity)))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '(foo bar)))

(test-equal 3
  (((lambda (mk-length)
      (mk-length
       (mk-length
        (mk-length
         (mk-length eternity)))))
    (lambda (len)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1 (len (cdr l))))))))
   '(foo bar piyo)))

(test-end "mk-length_<=3-test")

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l)
       0)
      (else
       (add1
        ((mk-length eternity)
         (cdr l))))))))

(test-begin "mk-length_<=1-test-again")

(test-equal 1
  (((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (lambda (l)
        (cond
         ((null? l)
          0)
         (else
          (add1
           ((mk-length eternity)
            (cdr l))))))))
   '(apple)))

(test-end "mk-length_<=1-test-again")

(test-begin "mk-length-test-again")

(test-equal 5
  (((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (lambda (l)
         (cond
          ((null? l)
           0)
          (else
           (add1
            ((mk-length mk-length)
             (cdr l))))))))
   '(a b c d e)))

(test-end "mk-length-test-again")

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
