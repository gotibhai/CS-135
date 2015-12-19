;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 08, Problem 4
;;***************************************************
;;

;;PART A---------------------------------------------------------------

;;(avg-without-recursion lon) consumes lon (list of natural numebers)
;;  and produces a list of three elements where the 1st element is the average of
;;  all nums, 2nd is avg of all even nums and third is avg of all odd nums.
;;
;;avg-without-recursion: (listof Nat) -> (listof Num Num Num)
;; requires: list must contain at least one even and one odd natural number.
;;
;;Examples:
(check-expect (avg-without-recursion (list 2 3))
(list 2.5 2 3))

(check-expect (avg-without-recursion (list 1 2 3 4))
(list 2.5 3 2))


(define (avg-without-recursion lon)
 
  (list (/ (foldr + 0 lon) (length lon))
        (/ (foldr + 0 (filter even? lon)) (length (filter even? lon)))
        (/ (foldr + 0 (filter odd? lon)) (length (filter odd? lon)))))

;;Tests:
(check-expect (avg-without-recursion (list 5 2 3 5 6 7 9 25))
              (list 7.75 4 9))

(check-expect (avg-without-recursion (list 5 0 0 5 6 0 9 0))
              (list 3.125 1.2 19/3))

(check-expect (avg-without-recursion (list 5 0 0 5 0 0 9 0))
              (list 2.375 0 19/3))

(check-expect (avg-without-recursion (list 1 1 1 1 2 2 2 2))
              (list 1.5 2 1))





















   