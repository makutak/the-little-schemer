(define-module (util)
  #:export (atom?
            lat?
            member?
            add1
            sub1
            o+
            o-
            o*
            o^
            o<
            o>
            o=
            o/
            eqan?
            multirember
            firsts
            seconds
            eqlist?
            equal??
            1st-sub-exp
            2nd-sub-exp
            operator
            even??
            pick
            first
            second
            third
            build
            a-pair?
            revpair
            one?))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (o* n (sub1 m)))))))

(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n (o^ n (sub1 m)))))))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else
      (add1 (o/ (o- n m) m))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else
      (eq? a1 a2)))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat)
            (multirember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (car l))
            (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (car (cdr (car l)))
            (seconds (cdr l)))))))

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

(define equal??
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else
      (eqlist? s1 s2)))))


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define even??
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n))
      (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (a1 a2)
    (cons a1
          (cons a2 '()))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define revpair
  (lambda (pair)
    (build
     (second pair)
     (first pair))))

(define one?
  (lambda (n)
    (= n 1)))
