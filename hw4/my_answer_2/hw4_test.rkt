#lang racket

(require "hw4.rkt")

;; using racket's unit test module
(require rackunit)

;; arg : step size, ini : initial value
(define (stream-maker-my fn arg ini)
  (define (f x)
    (cons x (lambda () (f (fn x arg)))))
  (lambda () (f ini)))
(define ones (stream-maker-my (lambda (x y) 1) 0 1))
(define nats (stream-maker-my + 1 1))
(define powers-of-two (stream-maker-my * 2 2))
(define evens (stream-maker-my + 2 0))
(define odds (stream-maker-my + 2 1))

;; do not use # !
(define vec-test (vector (cons 1 2) (cons 2 3) 100 (cons 3 4) (cons 4 5) (cons 5 6)))
(define vec-test2 (vector '(1 2 3) '(2 3 4) 100 '(3 4 5) '(4 5 6) '(5 6 7)))
(define vec-test3 (vector (cons "a" 1) (cons "b" 2) 100 (cons "c" 3) (cons "d" 4)))
(define vec-test4 (vector (cons 'a 1) (cons 'b 2) 100 (cons 'c 3) (cons 'd 4)))

;; for problem 10
(define xs-test (list (cons 1 2) (cons 9 10) (cons 3 4) (cons 7 8) (cons 5 6)))

(define tests
  (test-suite
   "Tests for Assignment 4"
   
   ;; Problem 1
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5))
   (check-equal? (sequence 1 2 3) (list 1))
   (check-equal? (sequence 1 10 1) (list 1 2 3 4 5 6 7 8 9 10))
   (check-equal? (sequence 1 10 2) (list 1 3 5 7 9))
   
   ;; Problem 2
   (check-equal? (string-append-map
                  (list "dan" "dog" "curry" "dog2")
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg"))
   ;; Problem 3
   (check-equal? (list-nth-mod (list "sml" "racket" "ruby") 0) "sml")
   (check-equal? (list-nth-mod (list "sml" "racket" "ruby") 1) "racket")
   (check-equal? (list-nth-mod (list "sml" "racket" "ruby") 2) "ruby")
   (check-equal? (list-nth-mod (list "sml" "racket" "ruby") 3) "sml")
   (check-equal? (list-nth-mod (list "sml" "racket" "ruby") 4) "racket")
   ;; Problem 4
   (check-equal? (stream-for-n-steps ones 5) (list 1 1 1 1 1))
   (check-equal? (stream-for-n-steps nats 5) (list 1 2 3 4 5))
   (check-equal? (stream-for-n-steps powers-of-two 5) (list 2 4 8 16 32))
   (check-equal? (stream-for-n-steps evens 5) (list 0 2 4 6 8))
   (check-equal? (stream-for-n-steps odds 5) (list 1 3 5 7 9))
   ;; Problem 5
   (check-equal? (stream-for-n-steps funny-number-stream 20)
                 (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16 17 18 19 -20))
   ;; Problem 6
   (check-equal? (stream-for-n-steps dan-then-dog 5)
                 (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))
   ;; Problem 7
   (check-equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 5)
                 (list (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 -5)))
   ;; Problem 8
   (check-equal? (stream-for-n-steps (cycle-lists '(1 2 3) '("a" "b")) 5)
                 (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a")))
   ;; Problem 9
   (check-equal? (vector-assoc 1 vec-test) (cons 1 2))
   (check-equal? (vector-assoc 3 vec-test) (cons 3 4))
   (check-equal? (vector-assoc 6 vec-test) #f)
   (check-equal? (vector-assoc 1 vec-test2) (cons 1 (cons 2 (cons 3 null))))
   (check-equal? (vector-assoc 3 vec-test2) (cons 3 (cons 4 (cons 5 null))))
   (check-equal? (vector-assoc 6 vec-test2) #f)
   (check-equal? (vector-assoc "a" vec-test3) (cons "a" 1))
   (check-equal? (vector-assoc "b" vec-test3) (cons "b" 2))
   (check-equal? (vector-assoc "c" vec-test3) (cons "c" 3))
   (check-equal? (vector-assoc "d" vec-test3) (cons "d" 4))
   (check-equal? (vector-assoc "e" vec-test3) #f)
   (check-equal? (vector-assoc 'a vec-test4) (cons 'a 1))
   (check-equal? (vector-assoc 'b vec-test4) (cons 'b 2))
   (check-equal? (vector-assoc 'c vec-test4) (cons 'c 3))
   (check-equal? (vector-assoc 'd vec-test4) (cons 'd 4))
   (check-equal? (vector-assoc 'e vec-test4) #f)
   (check-equal? (vector-assoc "a" vec-test4) #f)
   
   ))
(require rackunit/text-ui)

;; In problem10 and 11, we mainly test them using print(display), so put the
;; test script outside the unit test.

;; Problem 10 (debug version)
(define cached-assoc-xs (cached-assoc xs-test 10))
(cached-assoc-xs 1)
(cached-assoc-xs 10)
(cached-assoc-xs 1)
(cached-assoc-xs 5)
(cached-assoc-xs 3)
(cached-assoc-xs 5)
(cached-assoc-xs 3)

;; Problem 11 (Challenge Problem)
(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a

;; runs the test
(run-tests tests)
