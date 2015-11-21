#lang racket

(provide (all-defined-out))

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
(struct eq-num (e1 e2) #:transparent)
;; the value of this language is either a const or a bool

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (let ([v (eval-exp (negate-e e))])
                       (if (const? v)
                           (const (- (const-int v)))
                           (error "negate applied to non-number")))]
        [(add? e) (let ([v1 (eval-exp (add-e1 e))]
                        [v2 (eval-exp (add-e2 e))])
                    (if (and (const? v1) (const? v2))
                        (const (+ (const-int v1) (const-int v2)))
                        (error "add applied to non-number")))]
        [(multiply? e) (let ([v1 (eval-exp (multiply-e1 e))]
                             [v2 (eval-exp (multiply-e2 e))])
                         (if (and (const? v1) (const? v2))
                             (const (* (const-int v1) (const-int v2)))
                             (error "multiply applied to non-number")))]
        [(bool? e) e]
        [(if-then-else? e) (let ([v1 (eval-exp (if-then-else-e1 e))])
                             (if (bool? v1)
                                 (if (bool-b v1)
                                     (eval-exp (if-then-else-e2 e))
                                     (eval-exp (if-then-else-e3 e)))
                                 (error "if-then-else applied to non-bool")))]
        [(eq-num? e) (let ([v1 (eval-exp (eq-num-e1 e))]
                           [v2 (eval-exp (eq-num-e2 e))])
                       (if (and (const? v1) (const? v2))
                           (if (eq? (const-int v1) (const-int v2))
                               (bool #t)
                               (bool #f))
                           (error "eq-num applied to non-number")))]
        [#t (error "eval-exp expected an exp")] ; not strictly necessary but helps debugging
        ))
