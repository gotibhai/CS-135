;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname A2Q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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
;;cs135-final-grade Nat Nat Nat Nat Nat -> Nat
;;
;;Examples:
(check-expect (cs135-final-grade 70 80 90 80 90) 84)
(check-expect (cs135-final-grade 50 60 90 45 90) 46)

(define (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade)
    
   (+(* 0.20 overall-assignment-grade)
    (* 0.10 first-midterm-grade)
    (* 0.20 second-midterm-grade)
    (* 0.05 participation-grade)
    (* 0.45 final-exam-grade)
    ))
              
(define (cs135-final-grade first-midterm-grade second-midterm-grade participation-grade
         overall-assignment-grade final-exam-grade)
  
(cond [(or (< overall-assignment-grade 50)
           (< (/(*(+(* 0.10 first-midterm-grade)(* 0.20 second-midterm-grade)) 10) 3) 50))

       (min 46 (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade))]
      
      [else (calculating-final-grade first-midterm-grade second-midterm-grade
         participation-grade overall-assignment-grade final-exam-grade)]))

;;Tests
(check-expect (cs135-final-grade 40 50 10 45 30) 37)



