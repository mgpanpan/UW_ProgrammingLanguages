#lang racket

(require "delay_force.rkt")

(define (wait-out)
  (define (iter n)
    (if (= n 100000000)
        10
        (iter (+ n 1))))
  (iter 0))

;;(my-mult 10 (wait-out)) ;; about 2 seconds
;;(my-mult 0 (wait-out)) ;; about 2 seconds


;;(my-mult-th 0 (lambda () (wait-out))) ;; very fast
;;(my-mult-th 1 (lambda () (wait-out))) ;; about 2 seconds
;;(my-mult-th 2 (lambda () (wait-out))) ;; about 4 seconds
;;(my-mult-th 3 (lambda () (wait-out))) ;; about 6 seconds

;;(my-mult-df 0 (my-delay (lambda () (wait-out)))) ;; very fast
;;(my-mult-df 1 (my-delay (lambda () (wait-out)))) ;; about 2 seconds
;;(my-mult-df 3 (my-delay (lambda () (wait-out)))) ;; about 2 seconds
