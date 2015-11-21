#lang racket

(require "macro_examples.rkt")

(define (factorial x)
  (my-if (= x 0)
         then 1
         else (* x (factorial (- x 1)))))

(factorial 3)
(factorial 5)
(factorial 10)

(define x 1)
(comment-out (+ x 1) (+ x 10))

(define (my-mult x y-promise)
  (cond [(= x 0) 0]
        [(= x 1) (my-force y-promise)]
        [#t (+ (my-force y-promise)
               (my-mult (- x 1) y-promise))]))

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 100000000) y)))

(my-mult 100000 (my-delay (slow-add 3 4)))

(define p (my-delay (begin (print "hi") (* 3 4))))
(my-force p)
(my-force p)
(my-force p)
(my-force (begin (display "hi\n") (my-delay (* 3 4))))  ;?????
;(my-force (begin (print "hi") ))