#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(define-module (ch-08)
  #:use-module (lib util)
  #:export (rember-f-not-curry
            eq?-salad
            eq?-c
            rember-f
            insertL-f
            insertR-f
            insertL
            insertR
            subst
            rember2
            value
            value2
            multirember-f
            multiremberT
            multirember&co
            eq?-tuna
            a-friend
            new-frend
            last-friend
            latest-friend
            multiinsertLR
            multiinsertLR&co
            evens-only*
            evens-only*&col
            the-last-friend))


(define rember-f-not-curry
  (lambda (test? a l)
    (cond
     ((null? a) '())
     ((test? (car l) a)
      (cdr l))
     (else
      (cons (car l)
            (rember-f-not-curry test? a (cdr l)))))))
;; memo:
;;
;; (lambda (a)
;;   (lambda (x)
;;     (eq? x a)))
;; => 引数としてaを渡されると、
;;    関数 (lambda (x)
;;           (eq? x a))
;;    を返す
;;    => カリー化と言う。


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;;kに'saladと名付ける
(define k 'salad)

;;(eq?-c k)という処理にeq?-saladと名付ける
(define eq?-salad (eq?-c k))

;;上記のことは、下記でできる
(let ((x 'salad)
      (y 'tuna))
  ((eq?-c x) y))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a)
        (cdr l))
       (else
        (cons (car l)
              ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons new
              (cons old
                    (cdr l))))
       (else
        (cons (car l)
              ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons old
              (cons new
                    (cdr l))))
       (else
        (cons (car l)
              ((insertR-f test?) new old (cdr l))))))))

;;上記関数は重複している箇所が多いので、まとめたい。
;;どこに挿入するかの箇所だけが違う。
;; new old l を受取り、適切なconsを実行する関数を作る。

(define seqL
  (lambda (new old l)
    (cons new
          (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old
          (cons new l))))

;;これらの関数を受け取る関数を作る。

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else
        (cons (car l)
              ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g seqL))

(define insertR
  (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new
           (cons  old
                  l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old
           (cons new
                 l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember2
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     (else
      (o^ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x 'o+) o+)
     ((eq? x 'o*) o*)
     (else o^))))

(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
        (operator nexp))
       (value2 (1st-sub-exp nexp))
       (value2 (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
        ((multirember-f test?) a (cdr lat)))
       (else
        (cons (car lat)
              ((multirember-f test?) a  (cdr lat))))))))

(define multirember-eq
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else
      (cons (car lat)
            (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-frend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
              seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
     (else
      (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L
                                R)))))))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even?? (car l))
        (cons (car l)
              (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))

(define evens-only*&col
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car  l))
      (cond
       ((even?? (car l))
        (evens-only*&col (cdr l)
                         (lambda (newl p s)
                           (col (cons (car l) newl)
                                (o* (car l) p) s))))
       (else
        (evens-only*&col (cdr l)
                         (lambda (newl p s)
                           (col newl
                                p (o+ (car l) s)))))))
     (else
      (evens-only*&col (car l)
                       (lambda (al ap as)
                         (evens-only*&col (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (o* ap dp)
                                                 (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))
