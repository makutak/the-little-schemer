#!/usr/local/bin/guile
!#

(add-to-load-path ".")
(define-module (ch-06)
  #:use-module (lib util)
  #:export (numbered?
            value
            value-prefix
            sero?
            edd1
            zub1
            .+))

;; memo: 算術式とは
;; 数を含むアトムか、
;; 2つの算術式を o+, o*, o^ で結合したものである。

;; 算術式の表現が、
;; o+, o*, o^ を除いて数だけを含んでいるか
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) 'o+)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o*)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o^)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'o+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o*)
      (o* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     (else
      (o^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))


(define value-prefix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     (else
      (o^ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define .+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else
      (edd1 (.+ n (zub1 m)))))))
