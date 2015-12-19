;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname estimate) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 06, Problem 2
;;***************************************************
;;


;; PART A -------------------------------------------------

(define random-number/big 4294967087)

;; (random-number min max) produces a random number between min and max
;; random-number: Num Num -> Num
;; Examples:
;; (random-number -1 1) -> 0.56
;; (random-number 0 0.5) -> 0.2

(define (random-number min max)
  
  (+ min (* (- max min) 
            (/ (random random-number/big) 
               random-number/big))))

;;(estimating-pi number m inside) uses accumulative recursion to
;;   produce the approximate value of pi using monte carlo simulation.
;;
;;estimating-pi: Nat Nat Nat -> Num
;; requires number > 0
;;

(define (estimating-pi number m inside)
  
  (cond [(= m number) (/ (* 4 inside) number)]
        [(<= (+ (expt (random-number -1 1) 2) (expt (random-number -1 1) 2))1)
         (estimating-pi number (add1 m) (add1 inside))]
        [else (estimating-pi number (add1 m) inside)]))


;;(estimate-pi number m inside) uses the helper function (estimating-pi) to
;;   produce the approximate value of pi using monte carlo simulation.
;;
;;estimate-pi: Nat Nat Nat -> Num
;; requires number > 0
;;

(define (estimate-pi number)
  
  (estimating-pi number 1 0))



