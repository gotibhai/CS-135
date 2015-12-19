;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (cs135-final-grade first-midterm-grade second-midterm-grade participation-grade overall-assignment-grade final-exam-grade)
(cond [(or (< overall-assignment-grade 50) (< (+(* 0.10 first-midterm-grade) (* 0.20 second-midterm-grade)) 50))
       (min 46 
  (+(* 0.20 overall-assignment-grade)
    (* 0.10 first-midterm-grade)
    (* 0.20 second-midterm-grade)
    (* 0.05 participation-grade)
    (* 0.45 final-exam-grade)
    ))]))