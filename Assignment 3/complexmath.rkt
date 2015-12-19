;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 4
;;***************************************************
;;

;;(posn-mult posn1 posn2)which consumes two Posns(posn1 and posn2)
;;and produces a Posn that is the result of the multiplication.
;;
;;posn-mult: Posn Posn -> Posn
;;
;;Examples:
(check-expect(posn-mult (make-posn 10 20) (make-posn 3 4)) (make-posn -50 100))

(define (posn-mult posn1 posn2)
  (make-posn
   (- (*(posn-x posn1) (posn-x posn2)) (*(posn-y posn1) (posn-y posn2)))
   (+ (*(posn-x posn1) (posn-y posn2)) (*(posn-y posn1) (posn-x posn2)))))

;;Tests:
(check-expect (posn-mult (make-posn 3 4)
                         (make-posn 5 7))
                         (make-posn -13 41))

(check-expect (posn-mult (make-posn 1 2)
                         (make-posn 3 4))
                         (make-posn -5 10))

(check-expect (posn-mult (make-posn 2.3 3.5)
                         (make-posn 1.6 7.4))
                          (make-posn -22.22 22.62))


;;***************************************************

;;(posn-div posn1 posn2)which consumes two Posns and produces the Posn
;;that is the result of dividing the first parameter by the second parameter.
;;
;;posn-div: Posn Posn -> Posn
;;     requires Posn2 != (make-posn 0 0)
;;
;;Examples:
(check-expect (posn-div (make-posn 5 5) (make-posn 5 5))  (make-posn 1 0))
(check-expect (posn-div (make-posn 100 10) (make-posn 0 10))  (make-posn 1 -10))

(define (posn-div posn1 posn2)
  (make-posn
  (/(+ (*(posn-x posn1) (posn-x posn2)) (*(posn-y posn1) (posn-y posn2)))
    (+(sqr(posn-x posn2)) (sqr(posn-y posn2))))
  (/(- (*(posn-y posn1) (posn-x posn2)) (*(posn-x posn1) (posn-y posn2)))
    (+(sqr(posn-x posn2)) (sqr(posn-y posn2))))))

;;Tests:
(check-expect (posn-div (make-posn 1 2) (make-posn 3 4)) (make-posn 0.44 0.08))
(check-expect (posn-div (make-posn 0 0) (make-posn 1 1)) (make-posn 0 0))

;;**************************************************

;;(rotate-along-circle angle posn2) is a function that consumes a number (angle)
;; greater than or equal to 0 and less than 2π and a Posn (posn2)(x, y) and generates
;; a posn obtained by rotating the original posn by the angle.
;;
;;rotate-along-circle: Num Posn -> Posn
;;      requires 0 <= Num < 2*Pi
;;
;;Examples:
(check-within (rotate-along-circle 4.712389 (make-posn 0.5 0))
              (make-posn 0 -0.5) 0.001)

(define (rotate-along-circle angle posn2)
  (posn-mult
   (make-posn
   (cos angle)
   (sin angle)) posn2))

;;Tests:
(check-within (rotate-along-circle 3.14 (make-posn 0 0.2))
              (make-posn #i-0.0003185305832973657 #i-0.1999997463455079) 0.001)
(check-within (rotate-along-circle 1.20 (make-posn 1 1))
              (make-posn #i-0.5696813314905527 #i1.2943968404439) 0.001)
(check-within (rotate-along-circle 1.20 (make-posn 0 0))
              (make-posn 0 0) 0.001)






   



  