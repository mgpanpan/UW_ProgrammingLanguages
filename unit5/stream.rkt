#lang racket

(provide (all-defined-out))
;(define ones (lambda () (cons 1 ones)))
;
;(define nats
;  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
;    (lambda () (f 1))))
;
;(define powers-of-two
;  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
;    (lambda () (f 2))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define ones (stream-maker (lambda (x y) 1) 1))
(define nats (stream-maker + 1))
(define powers-of-two (stream-maker * 2))

(define (number-until stream tester)
  (define (iter stream acc)
    (let ([pr (stream)])
      (if (tester (car pr))
          acc
          (iter (cdr pr) (+ acc 1)))))
  (iter stream 1))

;; arg : step size, ini : initial value
(define (stream-maker-my fn arg ini)
  (define (f x)
    (cons x (lambda () (f (fn x arg)))))
  (lambda () (f ini)))

(define evens (stream-maker-my + 2 0))
(define odds (stream-maker-my + 2 1))

(define (print-stream-until stream n)
  (define (iter stream cnt)
    (let ([pr (stream)])
      (if (= cnt n)
          (display "\n")
          (begin
            (display (car pr))
            (display " ")
            (iter (cdr pr) (+ cnt 1))))))
  (iter stream 0))