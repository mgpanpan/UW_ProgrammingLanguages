#lang racket

(require "stream.rkt")

ones
(ones)
(car (ones))
(car ((cdr (ones))))

nats
(nats)
(car (nats))
(car ((cdr (nats))))
(car ((cdr ((cdr (nats))))))
(car ((cdr ((cdr ((cdr (nats))))))))

powers-of-two
(powers-of-two)
(car (powers-of-two))
(car ((cdr (powers-of-two))))
(car ((cdr ((cdr (powers-of-two))))))
(car ((cdr ((cdr ((cdr (powers-of-two))))))))

;; (number-until ones (lambda (x) (= x 16)))   ;; infinite loop
(number-until nats (lambda (x) (= x 16)))
(number-until powers-of-two (lambda (x) (= x 16)))
(number-until powers-of-two (lambda (x) (> x 1000000000000000000000000000000000000000000000000000000000000000000000)))

(print-stream-until ones 10)
(print-stream-until nats 10)
(print-stream-until powers-of-two 10)
(print-stream-until evens 10)
(print-stream-until odds 10)
