;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sec12_expand-lon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (expp num x)
  (cond [(= x num) empty]
        [else (cons num (expp num (add1 x)))]))

(define (expand-lon ls)
  (cond [(empty? ls) empty]
        [else (cons (expp (first ls) 0) (expand-lon (rest ls)))]))

  