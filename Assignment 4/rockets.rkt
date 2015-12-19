;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rockets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 1
;;***************************************************
;;

;; PART A -------------------------------------------------

(define-struct stage (fuel-mass dry-mass thrust isp))
;;A Stage is a (make-stage Num Num Num Num)
;;        requires fuel-mass > 0
;;                 dry mass  > 0
;;                 thrust    > 0
;;                 isp       > 0
;;
;; A Rocket is a (listof Stage)

;; PART B -------------------------------------------------

;;(rocket-mass rocket) consumes a rocket and produces the total mass in
;;   tonnes of that rocket including the fuel.
;;
;;rocket-mass: Rocket -> Num
;;
;;Examples:
(check-expect (rocket-mass (cons (make-stage 90 30 1629.25 3333)
                                 (cons (make-stage 9 4 191.1 4500) empty))) 133)
(check-expect (rocket-mass (cons (make-stage 500 10 1700 5000)
                                 (cons (make-stage 20 5 200 3000) empty)))  535)

(define (rocket-mass rocket)
  
  (cond [(empty? rocket) 0]
        [else (+(stage-fuel-mass (first rocket))(stage-dry-mass
                (first rocket))(rocket-mass (rest rocket)))]))

;;Tests:
(check-expect (rocket-mass (cons (make-stage 5 50 1629.25 3333)
                                 (cons (make-stage 50 1 191.1 4500) empty)))106)
(check-expect (rocket-mass empty) 0)

;; PART C -------------------------------------------------

;;(rocket-twr rocket) consumes a rocket and produces a list of numbers.
;;    Each number is a thrust-to-weight ratio of that stage.
;;
;;rocket-twr: Rocket -> (listof Num)
;;
;;Examples:
(check-expect (rocket-twr (cons (make-stage 90 30 1629.25 3333)
                               (cons (make-stage 9 4 191.1 4500) empty)))
                               (cons 1.25 (cons 1.5 empty)))

(check-expect (rocket-twr (cons (make-stage 100 50 3920 3333)
                               (cons (make-stage 25 25 490 4500) empty)))
                               (cons 2 (cons 1 empty)))

;;Constant Definitions:
(define g 9.8)

(define (rocket-twr rocket)
  
  (cond [(empty? rocket) empty]
        [else (cons (/ (stage-thrust (first rocket))
                         (* (rocket-mass rocket) g))
                     (cons (/ (stage-thrust (first(rest rocket)))
                           (* (rocket-mass (rest rocket)) g)) empty))]))

;;Tests:
(check-expect(rocket-twr (cons (make-stage 500 100 24500 3333)
                               (cons (make-stage 300 100 7840 4500) empty)))
                               (cons 2.5 (cons 2 empty)))
(check-expect(rocket-twr empty) empty)


;; PART D -------------------------------------------------
                     
;;(rocket-delta-v rocket) produces the total delta v for that rocket.
;;
;;rocket-delta-v: Rocket -> Num
;;
;;Examples;
(check-within (rocket-delta-v (cons (make-stage 90 30 1629.25 3333)
                              (cons (make-stage 9 4 191.1 4500) empty))) 9067.40 1)

(check-within (rocket-delta-v (cons (make-stage 100 50 3920 3333)
                               (cons (make-stage 25 25 490 4500) empty))) 5429.42 1)

(define (rocket-delta-v rocket)
  
  (cond [(empty? rocket) 0]
        [else (+(* (stage-isp (first rocket))
        (- (log (rocket-mass rocket))
           (log (-(rocket-mass rocket)
           (stage-fuel-mass (first rocket))))))
           (rocket-delta-v (rest rocket)))]))

;;Tests
(check-within (rocket-delta-v (cons (make-stage 90 30 1629.25 3333)
                              (cons (make-stage 9 4 191.1 4500) empty))) 9067.40 1)

(check-within (rocket-delta-v (cons (make-stage 5 50 1629.25 3333)
                              (cons (make-stage 50 1 191.1 4500) empty))) 17854.26 1)

(check-within (rocket-delta-v empty) 0 1)




