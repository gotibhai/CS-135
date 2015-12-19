;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Sec12_sortlists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct auction (item id bid))
;;(make-auction String Number Number)


(define (sort-auction-records-by-bid lsar)
  (cond [(empty? lsar) empty]
        [else (insert (first lsar) (sort-auction-records-by-bid (rest lsar)))]))

(define (insert ele lsar)
  (cond [(empty? lsar) (cons ele empty)]
        [(> (auction-bid ele)(auction-bid (first lsar))) (cons ele lsar)]
        [else (cons (first lsar) (insert ele (rest lsar)))]))


 (sort-auction-records-by-bid (list (make-auction 2 2 1)(make-auction 2 2 10)(make-auction 2 2 100)))