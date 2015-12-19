;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2Q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 3
;;***************************************************
;;
;;(braverats x y) produces a symbol as an answer depending on the table outline provided in the question.
;;(braverats x y) Nat Nat -> Sym
;;
;;Examples :
(check-expect (braverats 1 1) 'X)
(check-expect (braverats 6 3) 'B)

(define (braverats x y)
( cond [(= x y) 'X]
       [(and(= x 1) (or(= y 7) (= y 3))) 'A]
       [(and(= y 1) (or(= x 7) (= y 3))) 'B]
       [(and(= x 0) (not(= y 5))) 'X]
       [(and(= y 0) (not(= x 5))) 'X]
       [(and(= x 3) (or(= y 6) (= y 4))) 'A]
       [(and(= x 3) (or(= y 2) (= y 1))) 'B]
       [(and(= x 2) (= y 3)) 'A]
       [(and(= x 4) (= y 3)) 'B]
       [(and(= x 6) (= y 3)) 'B]
       [(> x y) 'A]
       [(> y x) 'B] 
       ))

;;Tests
(check-expect (braverats 1 0) 'X)
(check-expect (braverats 0 5) 'B)
(check-expect (braverats 0 6) 'X)
(check-expect (braverats 1 7) 'A)
(check-expect (braverats 2 3) 'A)
(check-expect (braverats 2 1) 'A)
(check-expect (braverats 7 1) 'B)
(check-expect (braverats 3 6) 'A)
(check-expect (braverats 3 1) 'B)
(check-expect (braverats 4 3) 'B)




