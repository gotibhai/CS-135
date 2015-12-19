;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 4
;;***************************************************
;;

;;***************************************************
;;(indigestible serving-size fat-content carb-content protein-content) produces the number of grams
;;in the serving that are not fat, carbohydrate or protein.
;;
;;(indigestible serving-size fat-content carb-content protein-content) Nat Nat Nat Nat -> Int
;;
;;Examples:
(check-expect (indigestible 30 5 7 10) 8)
(check-expect (indigestible 30 10 12 10) -2)


(define fat-calories 9)
(define carb-calories 4)
(define protein-calories 4)

(define (indigestible serving-size fat-content carb-content protein-content)
  
  (- serving-size (+ fat-content carb-content protein-content)))

;;Tests
(check-expect (indigestible 20 5 5 5) 5)

;;***************************************************

;;(high-protein? serving-size fat-content carb-content protein-content) determines if the number of grams
;; of protein are at least five.
;;
;;(high-protein? serving-size fat-content carb-content protein-content) Nat Nat Nat Nat -> Bool
;;
;;Examples:
(check-expect(high-protein? 10 0 5 10) true)
(check-expect(high-protein? 20 4 5 4) false)

(define (high-protein? serving-size fat-content carb-content protein-content)
  
  (> protein-content 5))

;;Tests
(check-expect(high-protein? 10 2 3 3) false)

;;***************************************************

;;(calories serving-size fat-content carb-content protein-content) produces the number of calories in the food.
;;Each gram of fat produces nine calories, and carbohydrate and protein each produce four calories per gram.
;;
;;(calories serving-size fat-content carb-content protein-content) Nat Nat Nat Nat -> Nat
;;
;;Examples
(check-expect (calories 50 10 15 10) 190)
(check-expect (calories 40 10 5  10) 150)

(define (calories serving-size fat-content carb-content protein-content)
  
  (+(* fat-calories fat-content)
    (* protein-calories protein-content)
    (* carb-content carb-calories)))

;;Tests
(check-expect (calories 50 20 20  10) 300)

;;***************************************************

;;(low-carb? serving-size fat-content carb-content protein-content) determines if the number of grams of
;;carbo-hydrate are less than two, or the number of calories produced from carbohydrate is less
;;than or equal to 5% of the total calories.
;;
;;(low-carb? serving-size fat-content carb-content protein-content) Nat Nat Nat Nat -> Bool
;;
;;Examples
(check-expect (low-carb? 40 10 10 10) false)
(check-expect (low-carb? 40 10 1 10)   true)

(define (low-carb? serving-size fat-content carb-content protein-content)
  
  (or (< carb-content 2)
      (< (*(/ (* carb-calories carb-content)
              (calories serving-size fat-content carb-content protein-content))100) 5)))

;;Tests
(check-expect (low-carb? 40 20 3 17)  true)

;;***************************************************

;;(zone? serving-size fat-content carb-content protein-content) determines if a food is balanced
;;for the Zone (fad) diet. A food is “balanced” if the percent of calories
;;from (fat/carbohydrate/protein) are (30%/40%/30%).
;;
;;(zone? serving-size fat-content carb-content protein-content) Nat Nat Nat Nat -> Bool
;;
;;Examples
(check-expect(zone? 40 12 12 12)false)
(check-expect(zone? 50 15 20 15)false)


(define (zone? serving-size fat-content carb-content protein-content)
  
  (and(and (<= (*(/ (* fat-calories fat-content)
                    (calories serving-size fat-content carb-content protein-content))100) 32)
           (>= (*(/ (* fat-calories fat-content)
                    (calories serving-size fat-content carb-content protein-content))100) 28))
      (and (<= (*(/ (* carb-calories carb-content)
                    (calories serving-size fat-content carb-content protein-content))100) 42)
           (>= (*(/ (* carb-calories carb-content)
                    (calories serving-size fat-content carb-content protein-content))100) 38))
      (and (<= (*(/ (* protein-calories protein-content)
                    (calories serving-size fat-content carb-content protein-content))100) 32)
           (>= (*(/ (* protein-calories protein-content)
                    (calories serving-size fat-content carb-content protein-content))100) 28))))

;;Tests
(check-expect(zone? 50 7 20 15) true)
(check-expect(zone? 40 6 19 14) true)
(check-expect(zone? 30 10 10 10) false)

;;***************************************************
      
