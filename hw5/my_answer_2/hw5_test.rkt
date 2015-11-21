#lang racket

(require "hw5.rkt")

(require rackunit)

(define MUPL-pair-test
  (apair (add (int 1) (int 2))
         (apair (add (int 2) (int 3)) (aunit))))
(define variables (list (cons "racket" (int 10)) 
                        (cons "Emacs" (add (var "racket") (int 2)))))

(define tests
  (test-suite
   "Tests for Assignment 5"
   
   ;; Problem 1
   (check-equal? (racketlist->mupllist (list (int 3) (int 4)))
                 (apair (int 3) (apair (int 4) (aunit))))
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
                 (list (int 3) (int 4)))
   (check-equal? (mupllist->racketlist (racketlist->mupllist (list (int 3) (int 4))))
                 (list (int 3) (int 4)))
   
   ;; Problem 2
   (check-equal? (eval-exp (int 10)) (int 10))
   (check-equal? (eval-exp (add (int 10) (int 20))) (int 30))
   ; ifgreater test
   (check-equal? (eval-exp (ifgreater (int 20) (int 10)
                                      (add (int 10) (int 20)) 
                                      (add (int 10) (int 30))))
                 (int 30))
   (check-equal? (eval-exp (ifgreater (int 20) (int 50)
                                      (add (int 10) (int 20)) 
                                      (add (int 10) (int 30))))
                 (int 40))
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) 
                 (int 2))
   ; mlet test
   (check-equal? (eval-exp (mlet "racket" (int 1) 
                                 (add (var "racket") (int 2))))
                 (int 3))
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x"))))
                 (int 6))
   ; call test
   (check-equal? (eval-exp (call (fun "fun-test" "a" (add (var "a") (int 10)))
                                 (add (int 1) (int 2))))
                 (int 13))
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7)))
                                 (int 1)))
                 (int 8))
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) 
                                 (int 1))) 
                 (int 8))
   (check-equal? (eval-under-env (call (closure (list (cons "a" (int 2)))
                                                (fun "fun-lexical" "b"
                                                     (add (var "b") (var "a"))))
                                       (int 10)) (list (cons "a" (int 1))))
                 (int 12))
   ; apair test
   (check-equal? (eval-exp MUPL-pair-test)
                 (apair (int 3) (apair (int 5) (aunit))))
   ; fst test
   (check-equal? (eval-exp (fst MUPL-pair-test))
                 (int 3))
   ; snd test
   (check-equal? (eval-exp (snd MUPL-pair-test))
                 (apair (int 5) (aunit)))
   ; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit)))))
                 (int 0))
   (check-equal? (eval-exp (isaunit (snd (snd MUPL-pair-test))))
                 (int 1))
   
   ;; Problem 3
   ; ifaunit test
   (check-equal? (eval-exp (ifaunit (aunit) (add (int 1) (int 2))
                                    (add (int 1) (int 3))))
                 (int 3))
   (check-equal? (eval-exp (ifaunit (int 1) (add (int 1) (int 2))
                                    (add (int 1) (int 3))))
                 (int 4))
   ; mlet test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x")))
                 (int 10))
   (check-equal? (eval-exp (mlet* variables (add (var "racket") (var "Emacs"))))
                 (int 22))
   ; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
                 (int 4))
   (check-equal? (eval-exp (ifeq (int 10) (int 10) (add (int 1) (int 2))
                                 (add (int 1) (int 3))))
                 (int 3))
   (check-equal? (eval-exp (ifeq (int 20) (int 10) (add (int 1) (int 2))
                                 (add (int 1) (int 3))))
                 (int 4))
   
   ;; Problem 4
   ; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) 
                                 (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)))
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))
                                 MUPL-pair-test)) 
                 (apair (int 10) (apair (int 12) (aunit))))
   ; mupl-mapAddN test
   (check-equal? (eval-exp (call (call mupl-mapAddN (int 7)) (apair (int 1) (aunit))))
                 (apair (int 8) (aunit)))
   (check-equal? (eval-exp (call (call mupl-mapAddN (int 7)) MUPL-pair-test))
                 (apair (int 10) (apair (int 12) (aunit))))
   
   ;; Problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-mapAddN (int 7))
                                  (racketlist->mupllist 
                                   (list (int 3) (int 4) (int 9)))))) 
                 (list (int 10) (int 11) (int 16)))
   
   ))

(require rackunit/text-ui)

;; ------------------------------------------------------------------------
;; tests that will raise an error (1.run-time MUPL type error, 2.unbound MUPL variable)

;; unbound variable during evaluation "racket"
; (eval-exp (var "racket")) 

;; MUPL ifgreater, e1 and e2 not both are integers
;(eval-exp (ifgreater (int 20) (aunit)
;                     (add (int 10) (int 20))
;                     (add (int 10) (int 30))))
;; ------------------------------------------------------------------------


;; runs the test
(run-tests tests)
