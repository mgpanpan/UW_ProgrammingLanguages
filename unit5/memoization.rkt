#lang racket

(provide (all-defined-out))

;; recursive process
(define (fib_v1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fib_v1 (- x 1))
         (fib_v1 (- x 2)))))

(define (fib_v2 x)
  (define (iter a b cnt)
    (if (= cnt x)
        a
        (iter b (+ a b) (+ cnt 1))))
  (iter 1 1 1))

(define (fib_v3 x)
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans
                             (if (or (= x 1 ) (= x 2))
                                 1
                                 (+ (f (- x 1)) (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans)
                                           memo))
                          new-ans)))))])
    (f x)))
                