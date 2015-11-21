#lang racket

(require "simple_exp_v2.rkt")

(define test-exp (multiply (negate (add (const 2) (const 2))) (const 7)))

(define test-ans (eval-exp test-exp))