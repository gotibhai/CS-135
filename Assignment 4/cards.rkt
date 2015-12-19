;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 1
;;***************************************************
;;

;; PART A -------------------------------------------------

(define-struct card (strength speed intelligence charm))
;; A card is a (make-card Nat Nat Nat Nat)
;;             requires strength > 0
;;                         speed > 0
;;                  intelligence > 0
;;                         charm > 0

;; PART B -------------------------------------------------

;;(card-to-list card1) consumes a card and produces a list of strength 
;;
;;card-to-list: Card -> (listof Nat)
;;
;;Examples:
(check-expect (card-to-list (make-card 12 23 1 1))
              (cons 12 (cons 23 (cons 1 (cons 1 empty)))))
(check-expect (card-to-list (make-card 10 10 10 10))
              (cons 10 (cons 10 (cons 10 (cons 10 empty)))))

(define (card-to-list card1)
  
  (cons (card-strength card1)
        (cons  (card-speed card1)
               (cons (card-intelligence card1)
                     (cons (card-charm card1) empty)))))

;;Tests:
(check-expect (card-to-list (make-card 10 9 8 7))
              (cons 10 (cons 9 (cons 8 (cons 7 empty)))))
(check-expect (card-to-list (make-card 9 9 9 9))
              (cons 9 (cons 9 (cons 9 (cons 9 empty)))))

;;(list-to-card list1) consumes a list (list1) of four numbers
;;     and produces a card.
;;
;;list-to-card: (listof Nat) -> Card
;;
;;Examples:
(check-expect (list-to-card (cons 1(cons 2(cons 3(cons 4 empty)))))
              (make-card 1 2 3 4))
(check-expect (list-to-card (cons 1(cons 10(cons 10(cons 10 empty)))))
              (make-card 1 10 10 10))

(define (list-to-card list1)
  
  (make-card (first list1) (second list1) (third list1) (fourth list1)))

;;Tests:
(check-expect (list-to-card (cons 10(cons 100(cons 1000(cons 10000 empty)))))
              (make-card 10 100 1000 10000))
(check-expect (list-to-card (cons 9(cons 9(cons 9(cons 9 empty)))))
              (make-card 9 9 9 9))

;; PART C -------------------------------------------------

;;(sum-of-list list1) takes in a listof Num and
;;    calculates the sum of the numbers.
;;
;;sum-of-list: (listof Nat) -> Nat
;;
;;Examples:
(check-expect(sum-of-list (cons 1 (cons 2 (cons 3 empty)))) 6)
(check-expect(sum-of-list (cons 10 (cons 2 (cons 12 empty)))) 24)

(define (sum-of-list list1)
  
  (cond [(empty? list1) 0]
        [else (+ (first list1) (sum-of-list (rest list1)))]))

;;Tests:
(check-expect(sum-of-list (cons 0 (cons 1 (cons 69 empty)))) 70)
(check-expect(sum-of-list (cons 100 (cons 2 (cons 69 empty)))) 171)

;;(card-regular? card1) consumes a card and
;;     tells if its a regular card (Sum = 10 and all elements > 0) or not.
;;
;;card-regular: Card -> Bool
;;
;;Examples:  
(check-expect(card-regular? (make-card 10 3 4 2)) false) 
(check-expect(card-regular? (make-card 10 10 10 10)) false)

;;Defining Constants
(define regular-card-sum 10)

(define (card-regular? card1)
  
  (cond [(= (sum-of-list (card-to-list card1)) regular-card-sum)  true]
        [else false]))

;;Tests:
(check-expect(card-regular? (make-card  2 3 2 3))  true)
(check-expect(card-regular? (make-card  1 1 4 5))  false)

;; PART D -------------------------------------------------

;;(count-wins list1 list2) consumes 2 lists of 4 numbers 
;;     and returns how many wins does the first
;;     list have over the other!
;;
;;count-wins: (listof Card) (listof Card) -> Nat
;;
;;Examples:
(check-expect (count-wins (cons 2 (cons 2 (cons 3 (cons 3 empty))))
                          (cons 1 (cons 1 (cons 1 (cons 7 empty))))) 3)
(check-expect (count-wins (cons 1 (cons 2 (cons 3 (cons 4 empty))))
                          (cons 3 (cons 2 (cons 1 (cons 4 empty))))) 1)                         

(define (count-wins list1 list2)
  
  (cond [(empty? list1) 0]
        [(empty? list2) 0]
        [(> (first list1) (first list2))
         (add1 (count-wins (rest list1)(rest list2)))]
        [else (+ 0 (count-wins (rest list1) (rest list2)))]))

;;Tests:
(check-expect (count-wins empty empty) 0)
(check-expect (count-wins (cons 1 (cons 2 (cons 3 (cons 4 empty)))) empty) 0)
(check-expect (count-wins empty (cons 1 (cons 2 (cons 3 (cons 4 empty))))) 0)


;;(card-battle card1 card2) uses the count-wins function
;;   and determines which card wins. 
;;
;;card-battle: Card Card -> Sym
;;
;;Examples:
(check-expect (card-battle (make-card 2 2 2 4) (make-card 4 3 2 1)) 'lose)
(check-expect (card-battle (make-card 2 2 2 4) (make-card 2 2 2 4)) 'draw)

(define (card-battle card1 card2)
  
 (cond [(> (count-wins (card-to-list card1) (card-to-list card2))
           (count-wins (card-to-list card2) (card-to-list card1))) 'win]
       [(> (count-wins (card-to-list card2) (card-to-list card1))
           (count-wins (card-to-list card1) (card-to-list card2))) 'lose]
       [else 'draw]))

;;Tests:
(check-expect (card-battle (make-card 4 2 2 2) (make-card 7 1 1 1))
                                               'win)
(check-expect (card-battle (make-card 4 2 2 2) (make-card 7 1 1 1))
                                               'win)
(check-expect (card-battle (make-card 4 2 2 2) (make-card 4 2 2 2))
                                               'draw)
(check-expect (card-battle (make-card 1 2 2 2) (make-card 2 3 3 3))
                                               'lose)
(check-expect (card-battle (make-card 4 2 4 1) (make-card 6 1 1 2))
                                               'draw)
;; PART E ---------------------------------------------------

;;(card-select card-list opponent-card) selects a card from the card-list
;; which will beat the opponent card.
;;
;;card-select: (listof Card) Card -> (anyof Card Bool)
;;
;;Examples:
(check-expect (card-select (cons (make-card 4 4 1 1)
                                 (cons (make-card 4 3 1 2)
                                       (cons (make-card 3 5 1 1)
                                             (cons (make-card 3 4 2 1) empty))))
                                       (make-card 7 1 1 1)) (make-card 4 3 1 2))

(check-expect (card-select (cons (make-card 3 4 1 2)
                                 (cons (make-card 3 3 2 2)
                                       (cons (make-card 3 1 5 1)
                                             (cons (make-card 2 4 1 3) empty))))
                                                     (make-card 4 4 1 1)) false)

(define (card-select card-list opponent-card)
  
  (cond [(empty? card-list) false]
        [(symbol=? 'win (card-battle (first card-list) opponent-card))
         (first card-list)]
        [else (card-select (rest card-list) opponent-card)]))

;;Tests:
(check-expect (card-select (cons (make-card 5 2 2 1)
                                 (cons (make-card 5 2 1 2)
                                       (cons (make-card 5 1 2 2)
                                             (cons (make-card 4 3 2 1) empty))))
                           (make-card 2 4 2 2)) false)

(check-expect (card-select (cons (make-card 5 2 2 1)
                                 (cons (make-card 5 2 1 2)
                                       (cons (make-card 5 1 2 2)
                                             (cons (make-card 4 3 2 1) empty))))
                           (make-card 7 1 1 1)) (make-card 5 2 2 1))

(check-expect (card-select empty (make-card 7 1 1 1)) false)

;; PART F ---------------------------------------------------

;;(count-collector-cards list-of-cards) consumes a list of cards and returns
;;     the number of collector cards in the list!
;;
;;count-collector-cards: (listof Card) -> Nat
;;
;;Examples:
(check-expect (count-collector-cards (cons (make-card 5 1 1 1)
                                       (cons (make-card 5 2 1 2)
                                          (cons (make-card 5 1 4 2)
                                             (cons (make-card 3 3 2 1)
                                                           empty))))) 3)

(check-expect (count-collector-cards (cons (make-card 1 1 1 1)
                                       (cons (make-card 2 2 1 2)
                                          (cons (make-card 1 1 4 2)
                                             (cons (make-card 1 3 2 1)
                                                           empty))))) 4)

;;Define Constants:
(define min-sum 3)

(define (count-collector-cards list-of-cards)
  
  (cond [(empty? list-of-cards) 0]
        [(and(not(= regular-card-sum
                 (sum-of-list (card-to-list (first list-of-cards)))))
             (< min-sum (sum-of-list (card-to-list (first list-of-cards)))))
             (add1 (count-collector-cards (rest list-of-cards)))]
        [else (+ 0 (count-collector-cards (rest list-of-cards)))]))

;;Tests
(check-expect (count-collector-cards (cons (make-card 2 3 1 1)
                             (cons (make-card 1 2 3 1)
                                   (cons (make-card 5 5 5 5)
                                         (cons (make-card 1 1 1 1) empty))))) 4)

(check-expect (count-collector-cards (cons (make-card 1 3 1 1)
                             (cons (make-card 1 2 3 1)
                                   (cons (make-card 5 5 5 90)
                                         (cons (make-card 5 3 1 1) empty))))) 3)

(check-expect (count-collector-cards empty) 0)




                                         
                                   








                                                      
             
             
             






