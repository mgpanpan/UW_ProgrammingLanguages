#lang racket

(provide (all-defined-out))

(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(define (my-mult x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (my-mult (- x 1) y))]))

(define (my-mult-th x y-th)
  (cond [(= x 0) 0]
        [(= x 1) (y-th)]
        [#t (+ (y-th) (my-mult-th (- x 1) y-th))]))

(define (my-mult-df x y-p)
  (cond [(= x 0) 0]
        [(= x 1) (my-force y-p)]
        [#t (+ (my-force y-p) (my-mult-df (- x 1) y-p))]))
