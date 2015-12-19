;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fourdigitenum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 4
;;***************************************************
;;

(define-struct four-digit-nat(e1 e2 e3 e4))
;;A four-digit-nat is a (make-four-digit-nat Nat Nat Nat Nat)
;;           requires 0 <= e1 <= 9
;;                    0 <= e2 <= 9
;;                    0 <= e3 <= 9
;;                    0 <= e4 <= 9

;;(number struc) this uses the components of the four-digit-num structure(struc)
;; and converts it to a 4 digit number.
;;
;;number: Four-Digit-Nat -> Nat
;;
;;Examples:
(check-expect (number (make-four-digit-nat 1 3 4 5)) 1345)
(check-expect (number (make-four-digit-nat 8 4 2 1)) 8421)

(define (number struc)
  (+(* 1000 (four-digit-nat-e1 struc))
                 (* 100 (four-digit-nat-e2 struc))
                 (* 10 (four-digit-nat-e3 struc))
                 (four-digit-nat-e4 struc)))

;;Tests:
(check-expect (number (make-four-digit-nat 9 9 9 9)) 9999)
(check-expect (number (make-four-digit-nat 7 9 1 9)) 7919)

;;(num-to-struct value1) takes the input of the number (value1) and
;; converts it to the structure of type four-digit-num.
;;
;;num-to-struct: Nat -> Four-Digit-Nat
;;
;;Examples:
(check-expect (num-to-struct 2456) (make-four-digit-nat 2 4 5 6))
(check-expect (num-to-struct 3589) (make-four-digit-nat 3 5 8 9))

(define (num-to-struct  value1)
  
  (make-four-digit-nat(remainder(quotient value1 1000)10)
                      (remainder(quotient value1 100)10)
                      (remainder(quotient value1 10)10)
                      (remainder value1 10)))

;;Tests:
(check-expect (num-to-struct 4009) (make-four-digit-nat 4 0 0 9))
(check-expect (num-to-struct 6969) (make-four-digit-nat 6 9 6 9))

;;(next-num value1) which consumes a Four-Digit-Nat and produces the next larger Four-Digit-Nat.
;;
;;next-num: Four-Digit-Nat -> Four-Digit-Nat
;;
;;Examples:
(check-expect(next-num (make-four-digit-nat 3 4 5 6)) (make-four-digit-nat 3 4 5 7))
(check-expect(next-num (make-four-digit-nat 4 0 0 0)) (make-four-digit-nat 4 0 0 1))

(define (next-num x)
  
  (num-to-struct(+ 1 (number x))))

;;Tests:
(define value0 (make-four-digit-nat 2 4 5 6))
(define value2 (make-four-digit-nat 2 4 5 7))
(define value3 (make-four-digit-nat 9 9 9 9))
(define value4 (make-four-digit-nat 0 0 0 0))
(define value5 (make-four-digit-nat 0 0 0 1))

(check-expect(next-num value0) value2)
(check-expect(next-num value3) value4)
(check-expect(next-num value4) value5)
(check-expect(next-num (make-four-digit-nat 0 0 0 1)) (make-four-digit-nat 0 0 0 2))
(check-expect(next-num (make-four-digit-nat 9 0 0 1)) (make-four-digit-nat 9 0 0 2))
(check-expect(next-num (make-four-digit-nat 0 9 9 9)) (make-four-digit-nat 1 0 0 0))
(check-expect(next-num (make-four-digit-nat 0 0 9 9)) (make-four-digit-nat 0 1 0 0))
(check-expect(next-num (make-four-digit-nat 0 0 0 9)) (make-four-digit-nat 0 0 1 0))



 