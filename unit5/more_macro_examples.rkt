#lang racket

(provide (all-defined-out))

(define-syntax let2
  (syntax-rules ()
    [(let2 () body)
     body]
    [(let2 (var val) body)
     (let ([var val]) body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
       (let ([var2 val2])
         body))]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body) body]
    [(my-let* ([var0 val0]
               [var-rest val-rest] ...)
              body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))

(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (define (iter index)
         (if (> index h)
             #t
             (begin body (iter (+ index 1)))))
       (iter l))]))

(define-syntax for-full
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (define (iter index)
         (if (> index h)
             #t
             (begin (body index) (iter (+ index 1)))))
       (iter l))]))
