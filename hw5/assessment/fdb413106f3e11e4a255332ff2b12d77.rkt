;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

; (a)
(define (racketlist->mupllist xs)
  (if (list? xs)
      (if (null? xs)
          (aunit)
          (apair (car xs) (racketlist->mupllist (cdr xs))))
      (error "argument to racketlist->mupllist is not list")))

; (b)
(define (mupllist->racketlist xs)
  (if (apair? xs)
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))
      null))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to comparison of non-numbers")))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cl-env (closure-env v1)]
                      [cl-fun (closure-fun v1)]
                      [fun-name (fun-nameopt cl-fun)]
                      [fun-formal (fun-formal cl-fun)]
                      [fun-body (fun-body cl-fun)]
                      [cl-env-ext (if fun-name
                                      (cons (cons fun-name v1) cl-env)
                                      cl-env)]
                      [cl-env-ext-arg (cons (cons fun-formal v2) cl-env-ext)])
                 (eval-under-env fun-body cl-env-ext-arg))
               (error "MUPL call first expression didn't evaluate to closure")))]
        [(mlet? e)
         (eval-under-env 
          (mlet-body e) 
          (cons 
           (cons 
            (mlet-var e) 
            (eval-under-env (mlet-e e) env)
            ) 
           env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair expression")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair expression")))]
        [(aunit? e)
         e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(closure? e)
         e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? e1)
      e2
      e3))

(define (mlet* lstlst e2)
  (letrec ([f (lambda (lst env)
                (if (null? lst)
                    null
                    (cons 
                     (cons (car (car lst)) (eval-under-env (cdr (car lst)) env)) 
                     (f (cdr lst) env))))])
    (eval-under-env e2 (f lstlst null))))
    
    (define (ifeq e1 e2 e3 e4)
      (let ([v1 (eval-under-env e1 null)]
            [v2 (eval-under-env e2 null)]
            [v3 (eval-under-env e3 null)]
            [v4 (eval-under-env e4 null)])
        (if (and (int? v1) (int? v2))
            (if (= (int-num v1) (int-num v2))
                v3
                v4)
            (error "MUPL ifeq applied to non-integer in comparison"))))
    
    ;; Problem 4
    
    ;; these two racket functions jsut help me understand map function
    ;;----
    (define (racket-map f list)
      (if (null? list)
          null
          (cons (f (car list)) (racket-map f (cdr list)))))

    (define racket-map-currying
      (lambda (f)
        (lambda (list)
          (if (null? list)
              null
              (cons (f (car list)) ((racket-map-currying f) (cdr list)))))))

    ;;----
    ; (call mupl-map (fun #f "x" (add (var "x") (int 7))))
    (define mupl-map
      (fun "map-cur" "f" 
           (fun "map-loop" "list"
                (ifgreater (isaunit (var "list")) 
                           (int 0)
                           (aunit)
                           (apair (call (var "f") (fst (var "list"))) 
                                  (call (call (var "map-cur") (var "f")) (snd (var "list"))))))))
    
    (define mupl-mapAddN 
      (mlet "map" mupl-map
            (fun "fun" "i"
                 (call (var "map") (fun #f "x" (add (var "i") (var "x")))))))
    
    ;; Challenge Problem
    
    (struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function
    
    ;; We will test this function directly, so it must do
    ;; as described in the assignment
    (define (compute-free-vars e) "CHANGE")
    
    ;; Do NOT share code with eval-under-env because that will make
    ;; auto-grading and peer assessment more difficult, so
    ;; copy most of your interpreter here and make minor changes
    (define (eval-under-env-c e env) "CHANGE")
    
    ;; Do NOT change this
    (define (eval-exp-c e)
      (eval-under-env-c (compute-free-vars e) null))
    