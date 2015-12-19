;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname search-lolos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (search-lolos sym ls)
  (cond [(empty? ls) false]
        [(search sym (first ls)) true]
        [else (search-lolos sym (rest ls))]))

(define (search sym ls)
  (cond [(member? sym ls) true]
        [else false]))


(search-lolos 'A (list (list 'B 'C 'W) (list 'AZ 'F 'W)))