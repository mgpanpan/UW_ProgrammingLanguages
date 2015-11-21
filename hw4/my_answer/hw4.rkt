#lang racket

(provide (all-defined-out))

;; question 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; question 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; question 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t 
         (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]))

;; question 4
;; Method 1
;; iterative process
;(define (stream-for-n-steps s n)
;  (define (iter stream cnt acc)
;    (let ([pr (stream)])
;      (if (= cnt n)
;          acc
;          (iter (cdr pr) (+ cnt 1) (append acc (list (car pr)))))))
;  (iter s 0 null))

;; Method 2
;; recursive process
(define (stream-for-n-steps s n)
  (let ([pr (s)])
    (if (= n 0)
        null
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; question 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; question 6
(define dan-then-dog
  (letrec ([f (lambda (x y)
                (cons x (lambda () (f y x))))])
    (lambda () (f "dan.jpg" "dog.jpg"))))

;; question 7
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda () (cons (cons 0 (car pr))
                     (stream-add-zero (cdr pr))))))

;; question 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (cnt)
                (cons (cons (list-nth-mod xs cnt) (list-nth-mod ys cnt))
                      (lambda () (f (+ cnt 1)))))])
    (lambda () (f 0))))

;; question 9
(define (vector-assoc v vec)
  (letrec ([iter (lambda (cnt)
                   (cond [(= cnt (vector-length vec)) #f]
                         [(pair? (vector-ref vec cnt))
                          (if (equal? (car (vector-ref vec cnt)) v)
                              (vector-ref vec cnt)
                              (iter (+ cnt 1)))]
                         [#t (iter (+ cnt 1))]))])
    (iter 0)))

;; question 10
;(define (cached-assoc xs n)
;  (let ([cache (make-vector n #f)]
;        [index 0])
;    (lambda (v)
;      (let ([ans (vector-assoc v cache)])
;        (if ans
;            ans
;            (let ([new-ans (assoc v xs)])
;              (if new-ans
;                  (begin
;                    (vector-set! cache index new-ans)
;                    (if (= index (- n 1))
;                        (set! index 0)
;                        (set! index (+ index 1)))
;                    new-ans)
;                  new-ans)))))))

;; for test
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [index 0])
    (lambda (v)
      (let ([ans (vector-assoc v cache)])
        (if ans
            (begin
              (displayln "get from cache")
              ans)
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (vector-set! cache index new-ans)
                    (if (= index (- n 1))
                        (set! index 0)
                        (set! index (+ index 1)))
                    (displayln "get by caculate")
                    new-ans)
                  new-ans)))))))

;; Challenge Problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([terminate-cond e1]
              [iter (lambda () 
                      (if (< e2 terminate-cond)
                          (iter)
                          #t))])
       (iter))]))
