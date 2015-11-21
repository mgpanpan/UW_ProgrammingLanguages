
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below

(define (sequence low high stride)
  (if (>= high low)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: empty list")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(car(list-tail xs (remainder n (length xs))))]))
  
(define (stream-for-n-steps s n)
  (if (= n 0) 
      null
      (cons (car(s)) (stream-for-n-steps (cdr(s)) (- n 1)))))

(define funny-number-stream 
  (letrec ([f (lambda (x) 
                (if (= (remainder x 5) 0)
                    (cons (* x -1) (lambda() (f (+ x 1 ))))
                    (cons x (lambda() (f (+ x 1))))))])
    (lambda() (f 1))))
  
(define dan-then-dog
  (letrec ([dan (lambda() (cons "dan.jpg" dog))]
           [dog (lambda() (cons "dog.jpg" dan))])
    dan))

(define (stream-add-zero s)
  (lambda() (cons (cons 0 
                        (car (s))) 
                  (stream-add-zero (cdr(s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) 
                            (list-nth-mod ys x)) 
                      (lambda() (f (+ x 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (if (<= (+ x 1) (vector-length vec))
                    (if (and (pair? (vector-ref vec x)) 
                             (equal? v (car(vector-ref vec x)))) 
                        (vector-ref vec x)
                        (f (+ x 1)))
                    #f))])
    (f 0)))
                    
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [i 0]
          [f (lambda(x)
               (let ([ans (vector-assoc x memo)])
                 (if ans
                     ans
                     (let ([new-ans (assoc x xs)])
                       (begin
                         (vector-set! memo i new-ans)
                         (set! i (remainder (+ i 1) n))
                         new-ans)))))])  
  f))
  
  