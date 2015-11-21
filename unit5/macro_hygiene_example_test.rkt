#lang racket

(require "macro_hygiene_example.rkt")

(double1 (begin (print "hi") 17))

(double2 (begin (print "hi") 17))

(double3 (begin (print "hi") 17))

(double4 (begin (print "hi") 17))

;; hygiene characteristic1: in processing local variable in macro definition
(let ([zero 17])
  (double4 zero))

;; hygiene characteristic2: in processing free variable in macro definition
(let ([+ *])
  (double3 17))
