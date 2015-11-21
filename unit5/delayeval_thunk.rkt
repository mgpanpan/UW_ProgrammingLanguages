#lang racket

(provide (all-defined-out))

(define (my-if x y z)
  (if x (y) (z)))

(define (factorial x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () (* x (factorial (- x 1))))))
