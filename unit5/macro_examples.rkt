#lang racket

(provide (all-defined-out))

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

(define-syntax comment-out
  (syntax-rules ()
    [(comment-out e1 e2)
     e2]))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))

(define-syntax my-force
  (syntax-rules ()
    [(my-force e)
     (if (mcar e)
         (mcdr e)
         (begin
           (set-mcar! e #t)
           (set-mcdr! e ((mcdr e)))
           (mcdr e)))]))

;(define-syntax my-force
;  (syntax-rules ()
;    [(my-force e)
;     (let ([x e])
;       (if (mcar x)
;           (mcdr x)
;           (begin
;             (set-mcar! x #t)
;             (set-mcdr! x ((mcdr x)))
;             (mcdr x))))]))
