;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (total x)
  (* 2 x))
(define (cs-135-participation clicker-questions correct-questions incorrect-questions)
(* (/ (+(* 2 correct-questions) (* 1 incorrect-questions)) (* 0.75 (total clicker-questions))) 100))