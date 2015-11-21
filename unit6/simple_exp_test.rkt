#lang racket

(require "simple_exp.rkt")

(define test-exp (Multiply (Negate (Add (Const 2) (Const 2))) (Const 7)))

(define test-ans (eval-exp test-exp))