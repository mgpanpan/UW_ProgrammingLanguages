#lang racket

(provide (all-defined-out))

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]))

;; Problem 4

; -------------------------------------------------------------------------
;; Method 1
;; iterative process
;(define (stream-for-n-steps s n)
;  (define (iter s cnt acc)
;    (let ([p (s)])
;      (if (= cnt n)
;          acc
;          (iter (cdr p) (+ cnt 1) (append acc (list (car p)))))))
;  (iter s 0 null))
; -------------------------------------------------------------------------

;; Method 2 
;; recursive process
(define (stream-for-n-steps s n)
  (let ([p (s)])
    (if (= n 0)
        null
        (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

;; Problem 5
;; Form 1 : using letrec
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- x) x) 
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; -------------------------------------------------------------------------
;; Form 2 : using local define
;(define (funny-number-stream)
;  (define (f x)
;    (cons (if (= (remainder x 5) 0) (- x) x)
;          (lambda () (f (+ x 1)))))
;  (f 1))
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; the examples in the lecture notes can be converted to local define form.
;(define (nats)
;  (define (f x)
;    (cons x (lambda () (f (+ x 1)))))
;  (f 1))
; -------------------------------------------------------------------------

;; Problem 6

; -------------------------------------------------------------------------
;; Method 1
;(define dan-then-dog
;  (letrec ([lut (list "dan.jpg" "dog.jpg")]
;           [f (lambda (n)
;                (cons (list-nth-mod lut n)
;                      (lambda () (f (+ n 1)))))])
;    (lambda () (f 0))))
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
;; Method 1, local define form
;(define (dan-then-dog)
;  (let ([lut (list "dan.jpg" "dog.jpg")])
;    (define (f n)
;      (cons (list-nth-mod lut n)
;            (lambda () (f (+ n 1)))))
;    (f 0)))
; -------------------------------------------------------------------------

;; Method 2
(define dan-then-dog
  (letrec ([f (lambda (x y) (cons x (lambda () (f y x))))])
    (lambda () (f "dan.jpg" "dog.jpg"))))

;; Problem 7
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda () (cons (cons 0 (car pr))
                     (stream-add-zero (cdr pr))))))

;; Problem 8
(define (cycle-lists xs ys)
  (define (f n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

;; Problem 9
(define (vector-assoc v vec)
  (let ([len (vector-length vec)])
    (define (iter index)
      (if (= index len) #f
          (let ([elem (vector-ref vec index)])
            (if (pair? elem)
                (if (equal? (car elem) v)
                    elem
                    (iter (+ index 1)))
                (iter (+ index 1))))))
    (iter 0)))

;; Problem 10
; -------------------------------------------------------------------------
;; for debug, add some print expressions
;(define (cached-assoc xs n)
;  (let ([cache-vec (make-vector n #f)]  ;; cache-vec and slot-index becomes
;        [slot-index 0])      ;; the global variable of the returned function
;    (lambda (v)
;      (let ([ans (vector-assoc v cache-vec)])
;        (if ans
;            (begin 
;              (display "value to be associated: ") (displayln v)
;              (displayln "using cache")
;              (display "output: ") (displayln ans)
;              (display "the cache content now is: ") (displayln cache-vec)
;              (display "the cache slot-index now is: ") (displayln slot-index)
;              (display "\n"))
;            (let ([new-ans (assoc v xs)])
;              (if new-ans
;                  (begin
;                    (vector-set! cache-vec slot-index new-ans)
;                    (set! slot-index (if (= slot-index (- n 1)) 0 (+ slot-index 1)))
;                    (display "value to be associated: ") (displayln v)
;                    (displayln "direct assoc")
;                    (display "output: ") (displayln new-ans)
;                    (display "the cache content now is: ") (displayln cache-vec)
;                    (display "the cache slot-index now is: ") (displayln slot-index)
;                    (display "\n"))
;                  (begin
;                    (display "value to be associated: ") (displayln v)
;                    (displayln "direct assoc") 
;                    (display "output: ") (displayln new-ans)
;                    (display "the cache content now is: ") (displayln cache-vec)
;                    (display "the cache slot-index now is: ") (displayln slot-index)
;                    (display "\n")))))))))

; -------------------------------------------------------------------------

(define (cached-assoc xs n)
  (let ([cache-vec (make-vector n #f)]  ;; cache-vec and slot-index becomes
        [slot-index 0])      ;; the global variable of the returned function
    (lambda (v)
      (let ([ans (vector-assoc v cache-vec)])
        (if ans ans
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (vector-set! cache-vec slot-index new-ans)
                    (set! slot-index (if (= slot-index (- n 1)) 0 (+ slot-index 1)))
                    new-ans)
                  new-ans)))))))

;; Problem 11 (Challenge Problem)
(define-syntax while-less
  (syntax-rules (do)
    [(while e1 do e2)
     (letrec ([end-cond e1]
              [iter (lambda ()
                      (if (>= e2 end-cond)
                          #t
                          (iter)))])
       (iter))]))
