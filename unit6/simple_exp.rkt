#lang racket

(provide (all-defined-out))

;; constructor
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

;; testor
(define (Const? e) (eq? (car e) 'Const))
(define (Negate? e) (eq? (car e) 'Negate))
(define (Add? e) (eq? (car e) 'Add))
(define (Multiply? e) (eq? (car e) 'Multiply))

;; selector
(define (Const-int e) (cadr e))
(define (Negate-e e) (cadr e))
(define (Add-e1 e) (cadr e))
(define (Add-e2 e) (caddr e))
(define (Multiply-e1 e) (cadr e))
(define (Multiply-e2 e) (caddr e))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (Const (+ (Const-int (eval-exp (Add-e1 e)))
                            (Const-int (eval-exp (Add-e2 e)))))]
        [(Multiply? e) (Const (* (Const-int (eval-exp (Multiply-e1 e)))
                                     (Const-int (eval-exp (Multiply-e2 e)))))]
        [#t (error "eval-exp expected an exp")]))
