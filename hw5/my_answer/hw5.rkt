;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var       (string)         #:transparent)  ;; a variable, e.g., (var "foo")
(struct int       (num)            #:transparent)  ;; a constant number, e.g., (int 17)
(struct add       (e1 e2)          #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent)  ;; if e1 > e2 then e3 else e4
(struct fun       (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call      (funexp actual)  #:transparent)  ;; function call
(struct mlet      (var e body)     #:transparent)  ;; a local binding (let var = e in body)
(struct apair     (e1 e2)          #:transparent)  ;; make a new pair
(struct fst       (e)              #:transparent)  ;; get first part of a pair
(struct snd       (e)              #:transparent)  ;; get second part of a pair
(struct aunit     ()               #:transparent)  ;; unit value -- good for ending a list
(struct isaunit   (e)              #:transparent)  ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun)          #:transparent)

;; Problem 1 Warm-Up
;; (a)
(define (racketlist->mupllist racketlist)
  (if (null? racketlist)
      (aunit)
      (apair (car racketlist)
             (racketlist->mupllist (cdr racketlist)))))
;; (b)
(define (mupllist->racketlist mupllist)
  (if (aunit? mupllist)
      null
      (cons (apair-e1 mupllist)
            (mupllist->racketlist (apair-e2 mupllist)))))

;; Problem 2, Implementing the MUPL Language

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; for debug
(define (print-env env)
  (if (null? env)
      (display "\n")
      (begin
        (display (caar env))
        (display ": ")
        (display (cdar env))
        (display "\n")
        (print-env (cdr env)))))
      
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
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater e1 and e2 must both be number")))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let ([fun-closure (eval-under-env (call-funexp e) env)])
           (if (closure? fun-closure)
               (let* ([fun-in-closure (closure-fun fun-closure)]
                      [env-in-closure (closure-env fun-closure)]
                      [env-expand (cons (cons (fun-formal fun-in-closure)
                                              (eval-under-env (call-actual e) env))
                                       env-in-closure)])
                 (if (fun-nameopt fun-in-closure)
                     (let ([env-expand (cons (cons (fun-nameopt fun-in-closure) fun-closure)
                                             env-expand)])
                       (eval-under-env (fun-body fun-in-closure) env-expand));; not an anonymous function
                     (eval-under-env (fun-body fun-in-closure) env-expand)))
               (error "MUPL call not a function closure")))]
;        [(call? e)
;         (let ([fun-closure (eval-under-env (call-funexp e) env)])
;           (begin
;             (displayln "environment before call")
;             (print-env env)
;             (if (closure? fun-closure)
;                 (let* ([fun-in-closure (closure-fun fun-closure)]
;                        [env-in-closure (closure-env fun-closure)]
;                        [env-expand (cons (cons (fun-formal fun-in-closure)
;                                                (eval-under-env (call-actual e) env))
;                                          env-in-closure)])
;                   (if (fun-nameopt fun-in-closure)
;                       (let ([env-expand (cons (cons (fun-nameopt fun-in-closure) fun-closure)
;                                               env-expand)])
;                         (begin
;                           (displayln "environment after call")
;                           (print-env env-expand)
;                           (eval-under-env (fun-body fun-in-closure) env-expand)));; not an anonymous function
;                       (begin
;                         (displayln "environment after call")
;                         (print-env env-expand)
;                         (eval-under-env (fun-body fun-in-closure) env-expand))))
;                 (error "MUPL call not a function closure"))))]
        [(mlet? e)
         (let* ([v1 (eval-under-env (mlet-e e) env)]
                [env-expand (cons (cons (mlet-var e) v1) env)])
           (eval-under-env (mlet-body e) env-expand))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "fst must apply to an apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "snd must apply to an apair")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        ;; CHANGE add more cases here
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "s1" e1) (cons "s2" e2))
         (ifgreater (add (var "s1") (int 1)) (var "s2")
                    (ifgreater (add (var "s2") (int 1)) (var "s1")
                               e3 e4)
                    e4)))

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f"
       (fun "" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f") (fst (var "lst")))
                            (call (call (var "mupl-map") (var "f")) (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "" "i"
             (fun "" "lst"
                  (call (call (var "map") 
                              (fun #f "x" (add (var "x") (var "i")))) 
                        (var "lst"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(fun? e)]
        [#t e]))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater e1 and e2 must both be number")))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let ([fun-closure (eval-under-env-c (call-funexp e) env)])
           (if (closure? fun-closure)
               (let* ([fun-in-closure (closure-fun fun-closure)]
                      [env-in-closure (closure-env fun-closure)]
                      [env-expand (cons (cons (fun-formal fun-in-closure)
                                              (eval-under-env-c (call-actual e) env))
                                        env-in-closure)])
                 (if (fun-nameopt fun-in-closure)
                     (let ([env-expand (cons (cons (fun-nameopt fun-in-closure) fun-closure)
                                             env-expand)])
                       (eval-under-env-c (fun-body fun-in-closure) env-expand));; not an anonymous function
                     (eval-under-env-c (fun-body fun-in-closure) env-expand)))
               (error "MUPL call not a function closure")))]
        [(mlet? e)
         (let* ([v1 (eval-under-env-c (mlet-e e) env)]
                [env-expand-c (cons (cons (mlet-var e) v1) env)])
           (eval-under-env-c (mlet-body e) env-expand))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "fst must apply to an apair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "snd must apply to an apair")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        ;; CHANGE add more cases here
        [#t (error "bad MUPL expression")]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
