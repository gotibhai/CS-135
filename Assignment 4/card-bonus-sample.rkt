;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname card-bonus-sample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct card (strength speed intelligence charm))
;; A card is a (make-card Num Num Num Num)

(define (num-to-card value1)
(make-card (remainder(quotient value1 1000)10)
           (remainder(quotient value1 100)10)
           (remainder(quotient value1 10)10)
           (remainder value1 10)))

(define (check-if-sum-10 value1)
   (and(= 10 (+ (remainder(quotient value1 1000)10)
           (remainder(quotient value1 100)10)
           (remainder(quotient value1 10)10)
           (remainder value1 10)))
          (> (remainder(quotient value1 1000)10) 0)
          (> (remainder(quotient value1 100)10) 0)
          (> (remainder(quotient value1 10)10)  0)
          (> (remainder value1 10) 0)))

(define max-num 7111)

(define (make-all-cards number)
  (cond [(= 1000 number) empty]
        [(check-if-sum-10 number) (cons (num-to-card number) (make-all-cards (- number 1)))]
        [else (make-all-cards (- number 1))]))

(define (winning-list card (make-all-cards max-num))
  (cond [(empty? (make-all-cards max-num)) empty]
        [(symbol=? 'win (card-battle (first (make-all-cards max-num)) card))
         (cons (first (make-all-cards max-num)) (winning-list card (rest (make-all-cards max-num))))]
        [else (winning-list card (rest (make-all-cards max-num)))]))




