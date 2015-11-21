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
;; here, "source" programs refer to programs written in MUPL.
(struct closure (env fun) #:transparent)

;; Problem 1 Warm Up
(define (racketlist->mupllist rktl)
  (if (null? rktl)
      (aunit)
      (apair (car rktl) (racketlist->mupllist (cdr rktl)))))
(define (mupllist->racketlist mupll)
  (if (aunit? mupll)
      null
      (cons (apair-e1 mupll) (mupllist->racketlist (apair-e2 mupll)))))

;; Problem 2 Implementing the MUPL language

;; lookup a variable in an environment
;; Do NOT change this function
;; environment is a list of associated pairs
;; [(string1, value1), (string2, value2), ... ]
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
        ; all values evaluates to themselves
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater, e1 and e2 not both are integers")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)]
               [var-val (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons var-name var-val) env)))]
        [(call? e)
         (let ([v-fun (eval-under-env (call-funexp e) env)]
               [v-actual (eval-under-env (call-actual e) env)])
           (if (closure? v-fun)
               (let ([f-body (fun-body (closure-fun v-fun))]
                     [f-nameopt (fun-nameopt (closure-fun v-fun))]
                     [f-formal (fun-formal (closure-fun v-fun))]
                     [env-when-defined (closure-env v-fun)])
                 (eval-under-env f-body
                                 (if f-nameopt
                                     (cons (cons f-nameopt v-fun)
                                           (cons (cons f-formal v-actual) env-when-defined))
                                     ; anonymous function, cannot recursive call itself
                                     (cons (cons f-formal v-actual) env-when-defined))))
               (error "MUPL call, first parameter is not a closure")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([e-value (eval-under-env (fst-e e) env)])
                    (if (apair? e-value)
                        (apair-e1 e-value)
                        (error "MUPL fst, applied to non-pair")))]
        [(snd? e) (let ([e-value (eval-under-env (snd-e e) env)])
                    (if (apair? e-value)
                        (apair-e2 e-value)
                        (error "MUPL snd, applied to non-pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3 Expanding MUPL language

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst)
            (mlet* (cdr lstlst) e2))))

;; can not satisfy the condition that e1 and e2 are evaluated exactly only once
;(define (ifeq e1 e2 e3 e4)
;  (ifgreater (add (int 1) e1) e2
;             (ifgreater (add (int 1) e2) e1
;                        e3 e4)
;             e4))

;; ! using let-expression to avoid repeated computations
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "s1" e1) (cons "s2" e2))
         (ifgreater (add (var "s1") (int 1)) (var "s2")
                    (ifgreater (add (var "s2") (int 1)) (var "s1")
                               e3 e4)
                    e4)))

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f") (fst (var "lst")))
                            (call (call (var "mupl-map") (var "f"))
                                  (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"    ; do not need recursive
             (fun #f "lst"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i"))))
                        (var "lst"))))))
;; this version also works fine
;(define mupl-mapAddN 
;  (fun #f "i"    ; do not need recursive
;       (fun #f "lst"
;            (call (call mupl-map (fun #f "x" (add (var "x") (var "i"))))
;                  (var "lst")))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(var? e) e]
        [(add? e) (add (compute-free-vars (add-e1 e))
                       (compute-free-vars (add-e2 e)))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        
        ;; ------------------------------------------------------------
        ;; I did not work it out 
;        [(fun? e)
;         (letrec ([gen-freevars 
;                   (lambda ())])
;           
;           
;           (fun-challenge (fun-nameopt e) (fun-formal e)
;                          ()
;                          (gen-freevars (set))
;           )]
        ;; ------------------------------------------------------------
        [(ifgreater? e)
         (ifgreater (compute-free-vars (ifgreater-e1 e))
                    (compute-free-vars (ifgreater-e2 e))
                    (compute-free-vars (ifgreater-e3 e))
                    (compute-free-vars (ifgreater-e4 e)))]

        [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e))
                         (compute-free-vars (mlet-body e)))]
        [(call? e) (call (compute-free-vars (call-funexp e))
                         (compute-free-vars (call-actual e)))]
        [(apair? e) (apair (compute-free-vars (apair-e1 e))
                           (compute-free-vars (apair-e2 e)))]
        [(fst? e) (fst (compute-free-vars (fst-e e)))]
        [(snd? e) (snd (compute-free-vars (snd-e e)))]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
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
        ;; CHANGE add more cases here
        ; all values evaluates to themselves
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        
        ;; ! different from eval-under-env
        [(fun-challenge? e)
         (letrec ([freevars-set (fun-challenge-freevars e)]
                  [gen-env-subset 
                   (lambda (env)
                     (if (null? env)
                         null
                         (if (set-member? freevars-set (caar env))
                             (cons (car env) (gen-env-subset (cdr env)))
                             (gen-env-subset (cdr env)))))])
           (closure (gen-env-subset env) e))]

        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater, e1 and e2 not both are integers")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)]
               [var-val (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons var-name var-val) env)))]
        [(call? e)
         (let ([v-fun (eval-under-env-c (call-funexp e) env)]
               [v-actual (eval-under-env-c (call-actual e) env)])
           (if (closure? v-fun)
               (let ([f-body (fun-body (closure-fun v-fun))]
                     [f-nameopt (fun-nameopt (closure-fun v-fun))]
                     [f-formal (fun-formal (closure-fun v-fun))]
                     [env-when-defined (closure-env v-fun)])
                 (eval-under-env-c f-body
                                 (if f-nameopt
                                     (cons (cons f-nameopt v-fun)
                                           (cons (cons f-formal v-actual) env-when-defined))
                                     ; anonymous function, cannot recursive call itself
                                     (cons (cons f-formal v-actual) env-when-defined))))
               (error "MUPL call, first parameter is not a closure")))]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env)
                           (eval-under-env-c (apair-e2 e) env))]
        [(fst? e) (let ([e-value (eval-under-env-c (fst-e e) env)])
                    (if (apair? e-value)
                        (apair-e1 e-value)
                        (error "MUPL fst, applied to non-pair")))]
        [(snd? e) (let ([e-value (eval-under-env-c (snd-e e) env)])
                    (if (apair? e-value)
                        (apair-e2 e-value)
                        (error "MUPL snd, applied to non-pair")))]
        [(isaunit? e) (if (aunit? (eval-under-env-c (isaunit-e e) env))
                          (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
