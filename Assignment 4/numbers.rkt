;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 1
;;***************************************************
;;

;; PART A -------------------------------------------------

;;(sum-list list) consumes a list of numbers, and produces the sum of the
;;    elements of that list.
;;
;;sum-list: (listof Num) -> Num
;;
;;Examples:
(check-expect(sum-list (cons 1 (cons 2 (cons 3 (cons 4 empty))))) 10)
(check-expect(sum-list (cons 10 (cons 2 (cons 13 (cons 30 empty))))) 55)

(define (sum-list list)
  
  (cond [(empty? list) 0]
        [else (+ (first list) (sum-list(rest list)))]))

;;Tests:
(check-expect (sum-list (cons 100 ( cons 200 (cons 90 empty)))) 390)
(check-expect (sum-list (cons 10 ( cons 20 (cons 300 empty)))) 330)
(check-expect (sum-list (cons 0 ( cons 20 (cons 300 empty)))) 320)
(check-expect (sum-list empty) 0)

;; PART B --------------------------------------------------

;;(divide-list list divisor) consumes a list and a divisor
;;     and divides each element of the list by the non-zero integer.
;;
;;divide-list: (listof Num) Num -> (listof Num)
;;             requires divisor not equal to 0
;;
;;Examples:
(check-expect (divide-list (cons 2 (cons 4 (cons 6 (cons 8 empty)))) 2)
              (cons 1 (cons 2 (cons 3 (cons 4 empty)))))

(check-expect (divide-list (cons 1 (cons 2 (cons 3 (cons 4 empty)))) 5)
             (cons 0.2 (cons 0.4 (cons 0.6 (cons 0.8 empty)))))

(define (divide-list list divisor)
  
  (cond [(empty? list) empty]
        [else(cons (/ (first list) divisor)
                   (divide-list (rest list) divisor))]))

;;Tests:
(check-expect (divide-list (cons 1 (cons 2 (cons 3 (cons 4 empty)))) 10)
             (cons 0.1 (cons 0.2 (cons 0.3 (cons 0.4 empty)))))

(check-expect (divide-list (cons 0 (cons 0 (cons 0 (cons 0 empty)))) 2)         
             (cons 0 (cons 0 (cons 0 (cons 0 empty)))))

(check-expect (divide-list (cons -10 (cons -15 (cons -20 (cons -25 empty)))) 5)         
             (cons -2 (cons -3 (cons -4 (cons -5 empty)))))

(check-expect (divide-list (cons 10 (cons 15 (cons 20 (cons 25 empty)))) -5)         
             (cons -2 (cons -3 (cons -4 (cons -5 empty)))))

(check-expect (divide-list empty 2) empty)

;; PART C --------------------------------------------------

;;(normalize-list list) consumes a list and produces the list obtained when each
;;     element of the consumed list is diveded by the sum of the consumed list.
;;
;;normalize-list: (listof Num) -> (listof Num)
;;                requires: Num > 0
;;                         (sum-list list) > 0
;;                
;;Examples:
(check-expect(normalize-list (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
             (cons 0.1 (cons 0.2 (cons 0.3 (cons 0.4 empty)))))

(check-expect(normalize-list (cons 10 (cons 10 (cons 10 (cons 10 empty)))))
             (cons 0.25 (cons 0.25 (cons 0.25 (cons 0.25 empty)))))

(define (normalize-list list)
  
  (divide-list list (sum-list list)))

;;Tests:
(check-expect(normalize-list (cons 10 (cons 5 (cons 30 (cons 5 empty)))))
             (cons 0.2 (cons 0.1 (cons 0.6 (cons 0.1 empty)))))

(check-expect(normalize-list (cons 0 (cons 0 (cons 0 (cons 1 empty)))))
             (cons 0 (cons 0 (cons 0 (cons 1 empty)))))

(check-expect(normalize-list empty) empty)
             

;; PART D ---------------------------------------------------

;;(list-replace list target-number replacement-number) produces a new
;;    list equal to the old list except each target number is replaced
;;    with the replacement number.
;;
;;list-replace: (listof Num) Num Num -> (listof Num)
;;
;;Examples:
(check-expect(list-replace (cons 1 (cons 2
                           (cons 3 (cons 4
                           (cons 5 (cons 6 empty)))))) 2 9)
                           (cons 1 (cons 9
                           (cons 3 (cons 4
                           (cons 5 (cons 6 empty)))))))

(check-expect(list-replace (cons 0 (cons 2
                           (cons 0 (cons 4
                           (cons 0 (cons 6 empty)))))) 0 69)
                           (cons 69 (cons 2
                           (cons 69 (cons 4
                           (cons 69 (cons 6 empty)))))))

(define (list-replace list target-number replacement-number)
  
  (cond [(empty? list) empty]
        [(not(empty? list)) (cond [(equal? (first list) target-number)
                                   (cons replacement-number (list-replace (rest list)
                                               target-number replacement-number))]
                                  [else (cons (first list)
                                              (list-replace (rest list)
                                               target-number replacement-number))])]))

;;Tests:
(check-expect(list-replace (cons 1 (cons 2
                           (cons 3 (cons 4
                           (cons 4 (cons 6 empty)))))) 4 0)
                           (cons 1 (cons 2
                           (cons 3 (cons 0
                           (cons 0 (cons 6 empty)))))))

(check-expect(list-replace empty 4 0) empty)

;; PART E ----------------------------------------------------

;;(count-repeats list) consumes a list and produces
;;     the number of adjacent duplicates.
;;
;;count-repeats: (listof Int) -> Int
;;
;;Examples:

(check-expect(count-repeats (cons 1 (cons 1
                            (cons 2 (cons 2
                            (cons 3 (cons 3 empty))))))) 3)

(check-expect(count-repeats (cons 1 (cons 1
                            (cons 2 (cons 3
                            (cons 4 (cons 5 empty))))))) 1)

(define (count-repeats list)
  
  (cond [(empty? list) 0]
        [(empty? (rest list)) 0]
        [(not(empty? list)) (cond [(equal? (first list) (first (rest list)))
                                   (+ 1 (count-repeats (rest list)))]
                                  [else (+ 0 (count-repeats (rest list)))])]))

;;Tests:
(check-expect(count-repeats (cons 1 (cons 2
                            (cons 3 (cons 4
                            (cons 5 (cons 6 empty))))))) 0)

(check-expect(count-repeats (cons 0 (cons 0
                            (cons 2 (cons 2
                            (cons 3 (cons 9 empty))))))) 2)

(check-expect(count-repeats (cons 1 (cons 1
                            (cons 1 (cons 3
                            (cons 5 (cons 5 empty))))))) 3)

(check-expect (count-repeats empty) 0)

(check-expect (count-repeats (cons 4 empty)) 0)

