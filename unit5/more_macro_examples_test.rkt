#lang racket

(require "more_macro_examples.rkt")

(let2 () 4)
(let2 (x 5) (+ x 4))
(let2 (x 5 y 6) (+ x y))

(define (silly-double x)
  (my-let* ([x (+ x 3)]
            [y (+ x 2)])
           (+ x y -8)))
(silly-double 100)

(for 5 to 10 do (displayln "*"))

(for-full 5 to 10 do (lambda (x) (displayln x)))
