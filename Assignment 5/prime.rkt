;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 05, Problem 1
;;***************************************************
;;


;; PART A -------------------------------------------------

(define x 2)

;;(count-factors num x) counts the number of factors of num.
;;
;;count-factors: Nat Nat -> Nat
;;
;;Examples:
(check-expect (count-factors 10 x) 2)
(check-expect (count-factors 15 x) 2)

(define (count-factors num x)
  
  (cond [(= num x) 0]
        [(zero? (remainder num x)) (add1 (count-factors num (add1 x)))]
        [else (count-factors num (add1 x ))]))

;;Tests:
(check-expect (count-factors 2 x) 0)

(check-expect (count-factors 3 x) 0)


;;(prime? num) consumes a num produces true if that number
;;     is prime and false otherwise.
;;
;;prime?: Nat -> Bool
;;        requires num > 0
;;
;;Examples:
(check-expect (prime? 3) true)
(check-expect (prime? 4) false)
(check-expect (prime? 14) false)
               
(define (prime? num)
  
  (cond [(or(zero? num) (= num 1)) false]
        [(zero? (count-factors num x)) true]
        [else false]))

;;Tests
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)
(check-expect (prime? 10) false)
(check-expect (prime? 13) true)
(check-expect (prime? 11) true)
(check-expect (prime? 15) false)
(check-expect (prime? 12) false)


;; PART B -------------------------------------------------


;;(next-prime num) takes in a number (num) and produces the next prime number.
;;
;;next-prime: Nat -> Nat 
;;
;;Examples:
(check-expect (next-prime 3) 5)
(check-expect (next-prime 10) 11)

(define (next-prime num)
  (cond [(equal? true (prime? (add1 num ))) (add1 num)]
        [else (next-prime (add1 num))]))

;;Tests:
(check-expect (next-prime 11) 13)
(check-expect (next-prime 12) 13)
(check-expect (next-prime 1) 2)
(check-expect (next-prime 0) 2)
(check-expect (next-prime 2) 3)
(check-expect (next-prime 7) 11)
(check-expect (next-prime 100) 101)


;; PART C -------------------------------------------------


;;(prime-range num1 num2) consumes two numbers and produces a list of all
;;    prime numbers between those numbers inclusivly!
;;
;;prime-range: Nat Nat -> (listof Nat)
;;             requires num1 < num2 
;;
;;Examples:
(check-expect (prime-range 2 10) (list 2 3 5 7))
(check-expect (prime-range 6 11) (list 7 11))
(check-expect (prime-range 10 11) (list 11))


(define (prime-range num1 num2)
  
  (cond [(> num1 num2) empty]
        [(prime? num1) (cons num1 (prime-range (add1 num1) num2))]
        [else (prime-range (add1 num1) num2)]))

;;Tests:
(check-expect (prime-range 2 11) (list 2 3 5 7 11))
(check-expect (prime-range 1 2) (list 2))
(check-expect (prime-range 11 2) empty)
(check-expect (prime-range 1 10) (list 2 3 5 7))
(check-expect (prime-range 1 1) empty)
(check-expect (prime-range 2 12) (list 2 3 5 7 11))
(check-expect (prime-range 2 13) (list 2 3 5 7 11 13))
(check-expect (prime-range 5 7) (list 5 7))
(check-expect (prime-range 7 7) (list 7))
(check-expect (prime-range 11 12) (list 11))






