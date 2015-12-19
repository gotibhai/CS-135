;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tri-area-CS-TUT) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (tri-area value1 value2 value3)
  (abs(/(+(*(posn-x value1)(-(posn-y value2) (posn-y value3)))
      (*(posn-x value2)(-(posn-y value3) (posn-y value1)))
      (*(posn-x value3)(-(posn-y value1) (posn-y value2)))) 2)))