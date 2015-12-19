;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (cs135-grade-sofar 1mtg 2mtg pg ag)
  (* (+ (* 0.10 1mtg) (* 0.20 2mtg) (* 0.05 pg) (* 0.20 ag)) 20/11))

(define (cs135-final-exam sofar overall-grade)
  (/(- overall-grade (* 0.55 sofar)) 0.45))