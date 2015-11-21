#lang racket

(require "hw4.rkt")

;; test 1
(sequence 1 2 3)
(sequence 1 10 1)
(sequence 1 10 2)
(sequence 1 10 3)

;; test 2
(string-append-map (list "sml" "racket" "ruby") "-programming")

;; test 3
(define xs (list "sml" "racket" "ruby"))
(list-nth-mod xs 0)
(list-nth-mod xs 1)
(list-nth-mod xs 2)
(list-nth-mod xs 3)

; (list-nth-mod '() 1)
; (list-nth-mod xs -1)

;; test 4
(define (ones) (cons 1 ones))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(stream-for-n-steps ones 10)
(stream-for-n-steps nats 10)
(stream-for-n-steps powers-of-two 10)

;; test 5
(stream-for-n-steps funny-number-stream 20)

;; test 6
(stream-for-n-steps dan-then-dog 5)

;; test 7
(stream-for-n-steps (stream-add-zero funny-number-stream) 10)

;; test 8
(stream-for-n-steps (cycle-lists '(1 2 3) '("a" "b")) 20)

;; test 9
(define vec (vector '(1 2 3) '(2 3 4) 100 '(3 4 5) '(4 5 6) '(5 6 7)))
(vector-assoc 1 vec)
(vector-assoc 3 vec)
(vector-assoc 6 vec)

;; test 10
(define xs-test (list (cons 1 2) (cons 9 10) (cons 3 4) (cons 7 8) (cons 5 6)))
(define cached-assoc-xs (cached-assoc xs-test 20))
(cached-assoc-xs 1)
(cached-assoc-xs 10)
(cached-assoc-xs 1)
(cached-assoc-xs 5)
(cached-assoc-xs 3)
(cached-assoc-xs 5)
(cached-assoc-xs 3)

;; Challenge Problem
(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
