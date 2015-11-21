;; Programming Languages, https://class.coursera.org/proglang-003
;; Homework 5
;; Date: 2014-11-19

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem 1. (a)
;;
;; Precondition: lst is a list?
;;
;; Type: Racket list of mupl values -> mupl list of mupl values
(define (racketlist->mupllist lst)
  (cond [(null? lst) (aunit)]
        [(pair? lst) (apair (car lst)
                            (racketlist->mupllist (cdr lst)))]))

;; Problem 1. (b)
;;
;; Precondition: mlst is a mupl list
;;
;; Type: Mupl list of mupl values -> Racket list of mupl values
(define (mupllist->racketlist mlst)
  (cond [(aunit? mlst) null]
        [(apair? mlst) (cons (apair-e1 mlst)
                             (mupllist->racketlist (apair-e2 mlst)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Evaluate expression e under the environment env
;;
;; Precondition: e is a valid mupl expression
;;               env is a valid environment list
;;
;; Type: mupl-exp * (string * mupl-exp) list -> mupl-exp
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
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let* ([clos (eval-under-env (call-funexp e) env)]
                [argv (eval-under-env (call-actual e) env)])
           (if (closure? clos)
               ; There are two options for shadowing,
               ; if the function name is the same as the
               ; formal argument name:
               ; 1) Formal shadows nameopt: Racket semantics
               ;    (define (x x) x) (x 2) => 2
               ; 2) Nameopt shadows formal
               ;    not very useful, I think.
               ; 3) Raise an error
               ; Here we choose 1).
               (let* ([recname (fun-nameopt (closure-fun clos))]
                      [argname (fun-formal (closure-fun clos))]
                      [clenv (closure-env clos)]
                      [clenv1 (if recname
                                  (cons (cons recname clos) clenv)
                                  clenv)]
                      [clenv2 (cons (cons argname argv) clenv1)])
                 (eval-under-env (fun-body (closure-fun clos)) clenv2))
               (error "MUPL call applied non-closure")))]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [env1 (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) env1))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))
  
(define (eval-exp e)
  (eval-under-env e null))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem 3. (a)
;;
;; Precondition: None
;;
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; Problem 3. (b)
;;
;; Precondition: prlst is a valid environment list
;;               e is a valid mupl expression
;;
(define (mlet* prlst e)
  (foldr (λ (pr eacc) (mlet (car pr) (cdr pr) eacc)) e prlst))

;; Problem 3. (c)
;;
;; Precondition: e1 and e2 evaluate to mupl ints
;;
(define (ifeq e1 e2 e3 e4)
  (let ([x (var "_x")]
        [y (var "_y")])
    (mlet* (list (cons (var-string x) e1)
                 (cons (var-string y) e2))
           (ifgreater x y
                      e4
                      (ifgreater y x
                                 e4
                                 e3)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem 4. (a)
;;
;; Type: mupl-map : ('a -> 'b) -> 'a list -> 'b list
(define mupl-map
  ; Pseudo code for the implementation:
  ;
  ; val mupl-map =
  ;   let fun aux f = fn lst => if null? lst
  ;                             then null
  ;                             else f (hd lst) :: (aux f) (tl lst)
  ;   in aux end
  (fun "aux" "f"
       (fun #f "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f") (fst (var "lst")))
                            (call (call (var "aux") (var "f"))
                                  (snd (var "lst"))))))))
;; Problem 4. (b)
;;
;; Type: mupl-mapAddN : int -> int list -> int list
(define mupl-mapAddN 
  ; val mupl-mapAddN = (fn i => map (fn x => x + i))
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "x" (add (var "x") (var "i")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Challenge Problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; Some helper functions

; map-car f (x, y) = (f x, y)
(define (map-car f pr)
  (cons (f (car pr)) (cdr pr)))
;; Substitute fun-challenge
;; Traverses the exp tree once, and builds up the
;; exp-challenge while calculating free variables.
;; 
;; subs : exp -> exp-challenge * free-vars
(define (substitute e)
  (cond [(var? e) (cons e (set (var-string e)))]
        [(int? e) (cons e (set))]
        [(add? e)
         (let ([pr1 (substitute (add-e1 e))]
               [pr2 (substitute (add-e2 e))])
           (cons (add (car pr1) (car pr2))
                 (set-union (cdr pr1) (cdr pr2))))]
        [(ifgreater? e)
         (let ([pr1 (substitute (ifgreater-e1 e))]
               [pr2 (substitute (ifgreater-e2 e))]
               [pr3 (substitute (ifgreater-e3 e))]
               [pr4 (substitute (ifgreater-e4 e))])
           (cons (ifgreater (car pr1) (car pr2) (car pr3) (car pr4))
                 (set-union (cdr pr1) (cdr pr2) (cdr pr3) (cdr pr4))))]
        [(fun? e)
         (let* ([pr (substitute (fun-body e))]
                [free-vars1 (set-remove (cdr pr) (fun-formal e))]
                [free-vars2 (if (fun-nameopt e)
                                 (set-remove free-vars1 (fun-nameopt e))
                                 free-vars1)])
           (cons (fun-challenge (fun-nameopt e)
                                (fun-formal e)
                                (car pr)
                                free-vars2)
                 free-vars2))]
        [(call? e)
         (let ([pr1 (substitute (call-funexp e))]
               [pr2 (substitute (call-actual e))])
           (cons (call (car pr1) (car pr2))
                 (set-union (cdr pr1) (cdr pr2))))]
        [(mlet? e)
         (let ([pr1 (substitute (mlet-e e))]
               [pr2 (substitute (mlet-body e))])
           (cons (mlet (mlet-var e) (car pr1) (car pr2))
                 (set-union (cdr pr1)
                            ; let var = e in body
                            ; Var is not a free variable in the body
                            ; so remove it from the results.
                            (set-remove (cdr pr2) (mlet-var e)))))]
        [(apair? e)
         (let ([pr1 (substitute (apair-e1 e))]
               [pr2 (substitute (apair-e2 e))])
           (cons (apair (car pr1) (car pr2))
                 (set-union (cdr pr1) (cdr pr2))))]
        [(fst? e) (map-car fst (substitute (fst-e e)))]
        [(snd? e) (map-car snd (substitute (snd-e e)))]
        [(aunit? e) (cons e (set))]
        [(isaunit? e)
         (map-car isaunit (substitute (isaunit-e e)))]
        ; Let's handle the closures, since they are
        ; useful for testing with, like in eval-exp.
        ; The free variables of a closure are the
        ; free variables of it's body minus any
        ; variables bound in the environment.
        ; Note that hopefully there aren't any
        ; free variables in a closure, since that
        ; could potentially give an unbound variable
        ; during evaluation error.
        [(closure? e)
         (let ([pr (substitute (closure-fun e))])
           (cons (closure (closure-env e) (car pr))
                 (set-subtract (cdr pr)
                               (list->set (map (λ (elem) (car elem))
                                               (closure-env e))))))]))

(define (compute-free-vars e)
  (car (substitute e)))

;;
;; Precondition: e is a valid exp-challenge
;;               env is a valid environment list
;;
(define (eval-under-env-c e env)
  (define (build-env-from-free-vars env free-vars)
    ; Choice: If a function definition has free variables,
    ; which are not in the environment at the point of
    ; definition, should we error out at once, or wait and see,
    ; if those variables are actually reached?
    ; I choose the latter.
    (cond [(set-empty? free-vars) null]
          [(null? env) null]
          [(pair? env)
           (let ([pr (car env)])
             (if (set-member? free-vars (car pr))
               (cons pr (build-env-from-free-vars (cdr env)
                                   (set-remove free-vars (car pr))))
               (build-env-from-free-vars (cdr env) free-vars)))]))    
  ;;;; Function body begins ;;;;
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
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun-challenge? e)
         (closure (build-env-from-free-vars env
                             (fun-challenge-freevars e)) e)]
        [(call? e)
         (let* ([clos (eval-under-env-c (call-funexp e) env)]
                [argv (eval-under-env-c (call-actual e) env)])
           (if (closure? clos)
               (let* ([recname (fun-challenge-nameopt (closure-fun clos))]
                      [argname (fun-challenge-formal (closure-fun clos))]
                      [clenv (closure-env clos)]
                      [clenv+ (if recname
                                  (cons (cons recname clos) clenv)
                                  clenv)]
                      [clenv++ (cons (cons argname argv) clenv+)])
                 (eval-under-env-c (fun-challenge-body (closure-fun clos)) clenv++))
               (error "MUPL call applied non-closure")))]
        [(mlet? e)
         (let* ([v (eval-under-env-c (mlet-e e) env)]
                [env+ (cons (cons (mlet-var e) v) env)])
           (eval-under-env-c (mlet-body e) env+))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))