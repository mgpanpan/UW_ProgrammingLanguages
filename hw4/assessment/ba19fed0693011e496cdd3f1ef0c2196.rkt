
#lang racket

;1
(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

;3
(define (list-nth-mod xs n)
  (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (e) (error 'list-nth-mod "empty list"))]
                  [exn:fail?
                   (lambda (e) ((error 'list-nth-mod "negative number")))])
    
    (define x (remainder n (length xs)))
    (car (list-tail xs x))
  )
  )
;4 
(define powers-of-two
(letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
(lambda () (f 2))))

(define (stream-for-n-steps s n)
(letrec ([f (lambda (s lst n)
   (let ([pr (s)])
        (if (= n 0)
        lst
        (f (cdr pr) (append lst (list (car pr))) (- n 1)))))])
(f s null n)))

;5
(define funny-number-stream
(letrec ([f (lambda (x) (cons (if (= (modulo x 5) 0)
                                  (- x)
                                  x)
                              (lambda () (f (+ x 1)))))])
(lambda () (f 1))))

;6
(define dan-then-dog
(letrec ([g (lambda (s) (cons s
                              (lambda () (g (if (string=? s "dan.jpg")
                                  "dog.jpg"
                                  "dan.jpg")))))])
(lambda () (g "dan.jpg"))))

;7
(define (stream-add-zero s1)
(letrec ([f (lambda (x) 
              (let ([pr (x)]) 
              (cons (cons 0 (car pr)) 
                            (lambda() (f (cdr pr))) )))])
   
(lambda() (f s1))))

;8 
(define (cycle-lists xs ys)
(letrec ([g (lambda (l1 l2 t1 t2)
                             
                             (cond [(and (null? l1) (null? l2)) (cons (cons (car t1) (car t2)) (lambda () (g (cdr t1) (cdr t2) t1 t2)))]
                                   [(null? l1) (cons      
                                               (cons (car t1) (car l2))(lambda () (g (cdr t1) (cdr l2) t1 t2)))]
                                   [(null? l2) (cons      
                                               (cons (car l1) (car t2))(lambda () (g (cdr l1) (cdr t2) t1 t2)))]
                                   [#t  (cons      
                                        (cons (car l1) (car l2))(lambda () (g (cdr l1) (cdr l2) t1 t2)))])
             )])
(lambda () (g xs ys xs ys))))

;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec n)
           (cond [(null? vec) #f]
                 [(= n (vector-length vec)) #f]
                 [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v) 
                                                (vector-ref vec n) 
                                                (f v vec (+ n 1)))
                                                ]
                 [#t (f v vec (+ n 1))]
        ))])
(f v vec 0)))

;10
(define (cached-assoc xs n)
(letrec   ([memo (make-vector n #f)] 
        [counter 0]
   [f (lambda (v)
   (let ([ans (vector-assoc v memo)])      
   (if ans
    ans
    (let ([new-ans (assoc v xs)])
    (begin
    (vector-set! memo counter new-ans)
    (if (= counter (- n 1))
        (set! counter 0)
        (set! counter (+ counter 1)))
     new-ans)))))])
f))
