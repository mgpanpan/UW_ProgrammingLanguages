#lang racket

(require "exp.rkt")

(define test1 (multiply (negate (add (const 2)
                                     (const 2)))
                        (const 7)))

(define test2 (multiply (negate (add (const 2)
                                     (const 2)))
                        (if-then-else (bool #f)
                                      (const 7)
                                      (bool #t))))

; can not gracefully handle, but it's okay, beause the given AST is not legal.
(define non-test (multiply (negate (add (const #t)
                                        (const 2)))
                           (const 7)))

; Here are two Racket functions that given language-being-implemented syntax,
; produce language-being-implemented syntax
(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

; this one takes a Racket list of language-being-implemented syntax
; and makes language-being-implemented syntax
(define (list-product es)
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))

(define test (andalso (eq-num (double (const 4))
                              (list-product (list (const 2) (const 2) (const 1) (const 2))))
                      (bool #t)))

(define result (eval-exp test))
