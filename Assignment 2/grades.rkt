;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 2
;;***************************************************
;;
;;(cs135-final-grade
;; first-midterm-grade second-midterm-grade
;; participation-grade overall-assignment-grade final-exam-grade) produces the final exam grade for cs-135.
;;
;;cs135-final-grade Num Num Num Num Num -> Num
;;
;;Examples:
(check-expect (cs135-final-grade 70 80 90 80 90) 84)
(check-expect (cs135-final-grade 50 60 90 45 90) 46)

(define midterm-1-weight 0.10)
(define midterm-2-weight 0.20)
(define participation-grade-weight 0.05)
(define final-exam-grade-weight 0.45)
(define assignment-grade-weight 0.20)
(define exam-weight 0.75)

;;****************************************************

;;
;;(calculating-final-grade first-midterm-grade second-midterm-grade participation-grade
;; overall-assignment-grade final-exam-grade) calculates the final grade
;;
;;calculating-final-grade Num Num Num Num Num -> Num
;;
;;Examples:
(check-expect(calculating-final-grade 80 70 60 80 90) 81.5)
(check-expect(calculating-final-grade 80 80 60 50 40)   55)


(define (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade)
    
   (+ (* assignment-grade-weight overall-assignment-grade)
    (* midterm-1-weight first-midterm-grade)
    (* midterm-2-weight second-midterm-grade)
    (* participation-grade-weight participation-grade)
    (* final-exam-grade-weight final-exam-grade)
    ))

;;Tests
(check-expect(calculating-final-grade 80 90 60 55 75)  73.75)
(check-expect(calculating-final-grade 50 50 90 77 40)   52.9)

;;****************************************************

;;(weighted-exam-average first-midterm-grade second-midterm-grade final-exam-grade) calculates the weighted
;; exam grade
;;
;; weighted-exam-average Num Num Num -> Num
;;
;;Examples:
(check-expect(weighted-exam-average 70 80 90) 254/3)
(check-expect(weighted-exam-average 80 85 90) 262/3)

(define (weighted-exam-average first-midterm-grade second-midterm-grade final-exam-grade)
  (/(+(* midterm-1-weight first-midterm-grade)
    (* midterm-2-weight second-midterm-grade)
    (* final-exam-grade-weight final-exam-grade)) exam-weight))

;;Tests
(check-expect(weighted-exam-average 50 55 90) 226/3)
(check-expect(weighted-exam-average 70 55 95) 81)

;;****************************************************
              
(define (cs135-final-grade first-midterm-grade second-midterm-grade participation-grade
         overall-assignment-grade final-exam-grade)
  
(cond [(or (< overall-assignment-grade 50)
           (< (weighted-exam-average first-midterm-grade second-midterm-grade final-exam-grade) 50))

       (min 46 (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade))]
      
      [else (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade)]))

;;Tests
(check-expect (cs135-final-grade 40 50 10 45 30) 37)
(check-expect (cs135-final-grade 80 80 90 65 85) 79.75)

;;****************************************************



