#lang racket

(require "hw5.rkt")

;; Problem 1 Test
(racketlist->mupllist '((int 1) (int 2) (int 3) (int 4) (int 5)))

(mupllist->racketlist (racketlist->mupllist '((int 1) (int 2) (int 3) (int 4) (int 5))))

;; Problem 2 Test
(eval-exp (int 10))
;; (eval-exp (var "racket"))  ;; unbounded
(eval-exp (add (int 10) (int 20)))
(eval-exp (ifgreater (int 20) (int 10) 
                     (add (int 10) (int 20)) 
                     (add (int 10) (int 30))))
(eval-exp (ifgreater (int 20) (int 50) 
                     (add (int 10) (int 20)) 
                     (add (int 10) (int 30))))
(eval-exp (mlet "racket" (int 1) (add (var "racket") (int 2))))
(define fun-test (eval-exp (fun "fun-test" "a" (add (var "a") (int 10)))))
(eval-exp (call fun-test (add (int 1) (int 2))))

;; Problem 3 Test
(eval-exp (ifaunit (aunit) (add (int 1) (int 2))
               (add (int 1) (int 3))))
(eval-exp (ifaunit (int 1) (add (int 1) (int 2))
               (add (int 1) (int 3))))

(define variables (list (cons "racket" (int 10)) (cons "Emacs" (add (var "racket") (int 2)))))
(eval-exp (mlet* variables (add (var "racket") (var "Emacs"))))

(eval-exp (ifeq (int 10) (int 10) (add (int 1) (int 2))
      (add (int 1) (int 3))))
(eval-exp (ifeq (int 10) (int 20) (add (int 1) (int 2))
      (add (int 1) (int 3))))

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))
