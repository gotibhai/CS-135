;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define(k x)
  (* (/ 3 4) x))

(define ( cs135-participation total-clicker-questions correct-questions wrong-questions)
     (* (/ (+ (* 2(min (k total-clicker-questions)  correct-questions)) (* 1 (min (- (k total-clicker-questions) correct-questions) wrong-questions ))) (* (k total-clicker-questions) 2)) 100))