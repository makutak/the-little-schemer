(define-module (util)
  #:export (atom?
            lat?
            member?
            add1
            sub1))

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
