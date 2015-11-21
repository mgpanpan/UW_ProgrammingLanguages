#lang racket

(provide (all-defined-out))

(define (funny_sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny_sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs)) (funny_sum (cdr xs)))]
        [#t (error "expected number or string")]))
