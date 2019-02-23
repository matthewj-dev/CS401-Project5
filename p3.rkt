#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 
; Project 3 -- Exercises in Racket 	  
; 	  
; The goal of this lab is to write six/seven functions in Racket.  	
;  	
; Each is described in comments below. The last is optional for CS 401. 	  
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide list-reverse
         list-append 	  
         zip-lists	
         slice-list 
         palindrome? 	  
         interp-arith 
         reduce-lambda) 	  

 	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Honor pledge (please write your name.)  	
;
; I **Matthew Jackson** have completed this code myself, without 
; unauthorized assistance, and have followed the academic honor code. 
;	
; Edit the code below to complete your solution.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define a list-reverse function that takes a list to its reverse
;;; e.g., (list-reverse '(1 2 3)) => '(3 2 1) 	  
(define (list-reverse lst) 	  
  (if (null? lst) null (append (list-reverse (cdr lst)) (list (car lst)))))  	


; Define an list-append function which takes two lists and returns their concatenation
;;; e.g., (list-append '(1) '(2 3)) => '(1 2 3)
(define (list-append lst0 lst1)	
  (foldr (lambda (roll into) (cons roll into)) lst1 lst0))  	


; Define a zip-lists function that takes multiple lists and zips them together into 	  
; a list of lists where each inner list is a list of all ith elements from input lists  	
;;; e.g., (zip-lists '(1 2 3) '(a b c)) => '((1 a) (2 b) (3 c))  	
; zipping should stop at the end of the shortest list and should support zipping	
; any number of lists together 	  
;;; e.g., (zip-lists '(1) '(2 3 4) '(5 6)) => '((1 2 5))  	
(define (zip-lists . args) 	  
  (if (or (null? args) (null? (car args)) (null? (car (cdr args))))
      '()
      (cons (append (list (car (car args))) (foldl (lambda (lst acc) (append acc (list (car lst)))) '() (cdr args)))
            (apply zip-lists (foldl (lambda (lst acc) (append acc (list (cdr lst)))) '() args)))))


; Define a slice-list function that takes a list, start position, and	
; end position to a list of elements between the given positions (inclusive) 
;;; e.g., (slice-list '(1 2 3 4 5) 2 3) => '(3 4) 	  
; any part of the requested list that is non-existent should be trimmed 	  
;;; e.g., (slice-list '(1 2) 1 4) => '(2)  	
; if the range of the slice is empty/nonexistent, then the result should be '() 
(define (slice-list lst n m)  	
  (if (>(+ n m 1) (length lst)) (drop lst n) (take (drop lst n) (+ m 1))))
  	

; Define a palindrome? predicate that only returns true for strings that are palindromes  	
;;; e.g., (palindrome? "abcdcba") => #t  	
(define (palindrome? s) 
  (equal? (string->list s) (reverse (string->list s))))
 	  

; Define function interp-arith---an arithmetic interpreter for the language:  	
; e ::= (+ e e) | (- e e) | (* e e) | (** e e) | n
; encoded as lists, where n is a number? and ** performs exponentiation	
;;; e.g., (interp-arith '(+ (* 2 3) (- 11 (** 2 3)))) => 9
(define (interp-arith e) 	  
  (match e
      [`(+ ,a ,b) (+ (interp-arith (car (cdr e))) (interp-arith (car (cdr (cdr e)))))]
      [`(- ,a ,b) (- (interp-arith (car (cdr e))) (interp-arith (car (cdr (cdr e)))))]
      [`(* ,a ,b) (* (interp-arith (car (cdr e))) (interp-arith (car (cdr (cdr e)))))]
      [`(** ,a ,b) (expt (interp-arith (car (cdr e))) (interp-arith (car (cdr (cdr e)))))]
      [ _ e])) 		 


; Define a function reduce-lambda, that takes a lambda term in language:  	
; e ::= (lambda (x) e) | (e e) | x   (encoded as a list; x is a symbol?) 
; and performs a single step of beta-reduction using CBN evaluation 	  
;;; e.g., (interp-lambda '((lambda (a) a) ((lambda (b) b) (lambda (c) c))))  	
;;;          => '((lambda (b) b) (lambda (c) c)) 
; This function is bonus for CS 401, required for CS 501 (assume correct input) 
(define (reduce-lambda exp) 
  'TODO) 	  




