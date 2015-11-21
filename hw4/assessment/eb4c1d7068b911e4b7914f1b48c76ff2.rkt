#lang racket
(provide (all-defined-out))

; problem 1
(define sequence 
  (lambda (l h s)
    (if (> l h) 
        null
        (cons l (sequence (+ l s) h s)))))

; problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) 
         (string-append x suffix))
       xs)) 

; problem 3
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; problem 4
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (cons (car(s)) (stream-for-n-steps (cdr(s)) (- n 1)))]))

; problem 5 
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (- x) x) 
                                (lambda() (f (+ x 1)))))])
    (lambda () (f 1))))

; problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons 
                          (if (odd? x) "dan.jpg" "dog.jpg")
                          (lambda () (f (+ x 1)))))])
         (lambda () (f 1))))

; problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons 
                           (cons 0 (car (x)))
                           (lambda () (f (cdr (x))))))])
  (lambda () (f s))))

; problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda(n) (cons
                         (cons (list-nth-mod xs n)
                               (list-nth-mod ys n))
                         (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; problem 9
(define (vector-assoc v vec)
  (define (helper n)
    (if (= n (vector-length vec))
        #f
        (let ([item (vector-ref vec n)])
          (cond [(not (pair? item)) (helper (+ n 1))]
                [(equal? v (car item)) item]
                [#t (helper (+ n 1))]))))
  (helper 0))


; problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)] ;iniitalize with all #f
           [iter 0] ; initialize iter to be 0
           [f (lambda (v)
              (cond [(vector-assoc v memo) (vector-assoc v memo)] 
                    ; if v in memo, return vector-assoc ...
                    
                    ; else if v (not in memo) in xs
                    [(assoc v xs) (begin
                                    ; set memo(iter) 
                                    (vector-set! memo iter (assoc v xs))
                                    
                                    ; update iter
                                    (set! iter (remainder (+ iter 1) n))
                                    
                                    ; return (assoc v xs)
                                    (assoc v xs))]
                                    
                                    ;otherwise return false
                                    [#t #f]))])
    f))
