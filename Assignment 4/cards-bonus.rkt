;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct card (strength speed intelligence charm))
;; A card is a (make-card Num Num Num Num)

;;(card-to-list card1) consumes a card and produces a list of four numbers.
;;
;;card-to-list: Card -> listof Num
;;
(define (card-to-list card1)
  (cons (card-strength card1)
        (cons  (card-speed card1)
               (cons (card-intelligence card1)
                     (cons (card-charm card1) empty)))))

;;(list-to-card list1) consumes a list(list1) of four numbers and produces a card.
;;
;;list-to-card: list1 -> Card
;;
(define (list-to-card list1)
  (make-card (first list1) (second list1) (third list1) (fourth list1)))

;;(sum-of-list list1) takes in a listof Num and calculates the sum of the numbers.
;;
;;sum-of-list: (listof Num) -> Num
;;
(define (sum-of-list list1)
  (cond [(empty? list1) 0]
        [else (+ (first list1) (sum-of-list (rest list1)))]))

;;(count-wins list1 list2) consumes 2 lists of 4 numbers and returns how many
;;wins does the first list have over the other!
;;
;;count-wins: listof Card listof Card -> Num
;;
(define (count-wins list1 list2)
  (cond [(empty? list1) 0]
        [(empty? list2) 0]
        [(> (first list1)  (first list2)) (+ 1 (count-wins (rest list1) (rest list2)))]
        [else (+ 0 (count-wins (rest list1) (rest list2)))]))

;;(card-battle card1 card2) uses the count-wins function and determines which
;; card wins.
;;
;;card-battle: Card Card -> Sym
(define (card-battle card1 card2)
 (cond [(> (count-wins (card-to-list card1) (card-to-list card2))
           (count-wins (card-to-list card2) (card-to-list card1))) 'win]
       [(> (count-wins (card-to-list card2) (card-to-list card1))
           (count-wins (card-to-list card1) (card-to-list card2))) 'lose]
       [else 'draw]))

;;(card-select card-list opponent-card) selects a card from the card list
;; which will beat the opponent card.
;;
;;card-select: (listof Card) -> (Anyof Card Bool)
(define (card-select card-list opponent-card)
  (cond [(empty? card-list) false]
        [(symbol=? 'win (card-battle (first card-list) opponent-card)) (first card-list)]
        [else (card-select (rest card-list) opponent-card)]))

(define (num-to-card value1)
(make-card (remainder(quotient value1 1000)10)
           (remainder(quotient value1 100)10)
           (remainder(quotient value1 10)10)
           (remainder value1 10)))

(define (check-if-valid value1)
   (and(= 10 (+ (remainder(quotient value1 1000)10)
           (remainder(quotient value1 100)10)
           (remainder(quotient value1 10)10)
           (remainder value1 10)))
          (> (remainder(quotient value1 1000)10) 0)
          (> (remainder(quotient value1 100)10) 0)
          (> (remainder(quotient value1 10)10)  0)
          (> (remainder value1 10) 0)))

(define max-num 7111)

(define (make-all-cards max-num)
  (cond [(= 1000 max-num) empty]
        [(check-if-valid max-num) (cons (num-to-card max-num) (make-all-cards (- max-num 1)))]
        [else (make-all-cards (- max-num 1))]))

(define (card-better card value)
  (card-with-better-value card value (winning-list card (make-all-cards max-num))))

(define (card-with-better-value card value winning-list)
  (cond [(empty? winning-list) false]
        [(cond [(and(symbol=? 'strength value) (> (card-strength (card-select winning-list card)) (card-strength card))) true]
               [(and(symbol=? 'speed value) (> (card-speed (card-select winning-list card)) (card-speed card))) true]
               [(and(symbol=? 'intelligence value) (> (card-intelligence (card-select winning-list card)) (card-intelligence card))) true]
               [(and(symbol=? 'charm value) (> (card-charm (card-select winning-list card)) (card-charm card))) true]
               [else false])(card-select winning-list card)]
        [else (card-with-better-value card value (rest winning-list))]))
        

(define (winning-list card all-regular-cards)
  (cond [(empty? all-regular-cards) empty]
        [(symbol=? 'win (card-battle (first all-regular-cards) card))
         (cons (first all-regular-cards) (winning-list card (rest all-regular-cards)))]
        [else (winning-list card (rest all-regular-cards))]))
   
(define (card-allbetter card )
  (winning-list card (make-all-cards max-num)))

