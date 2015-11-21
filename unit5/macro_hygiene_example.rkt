#lang racket

(provide (all-defined-out))

(define-syntax double1
  (syntax-rules ()
    [(double1 e)
     (* 2 e)]))

(define-syntax double2
  (syntax-rules ()
    [(double2 e)
     (+ e e)]))

(define-syntax double3
  (syntax-rules ()
    [(double3 e)
     (let ([x e])
       (+ x x))]))

(define-syntax double4
  (syntax-rules ()
    [(doubel4 e)
     (let* ([zero 0]
            [x e])
       (+ x x zero))]))
