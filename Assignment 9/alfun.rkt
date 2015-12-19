;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 09, Problem 4
;;***************************************************
;;

;;(singletons lon-1) produces a list of elements in lon-1 that
;; appear exactly once.
;;
;;singletons: (listof Num) -> (listof Num)
;;
;;Examples:
(check-expect (singletons '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
(check-expect (singletons '(8 1 1)) (list 8))

(define (singletons lon-1)
  (local
    [;;(how-many y ls) counts no of y in list(ls).
     ;;how-many: Num (listof Num) -> Nat
     (define (how-many y ls)
       (foldr (lambda (f cur)(cond [(equal? f y)(+ 1 cur)]
                      [else cur])) 0 ls))]
                
    (filter (lambda (y) (= (how-many y lon-1) 1)) lon-1)))


;;Tests:
(check-expect (singletons '(51)) (list 51))
(check-expect (singletons '(14 15)) (list 14 15))
(check-expect (singletons '(1.01 -1/2 -2/3)) (list 1.01 -1/2 -2/3))
(check-expect (singletons empty) empty)
(check-expect (singletons '(9 9 9)) empty)
(check-expect (singletons '(1 1 1 2 2 2 3 3 3 3 4 6 7 3)) (list 4 6 7))
(check-expect (singletons '(1 3 5 7 9 10 12)) (list 1 3 5 7 9 10 12))



;;PART B


;;(duplicates lon-1) makes a list of elements that appear > 1 
;; in lon-1. The final list contains only 1 of each element.
;;
;;duplicates: (listof Num) -> (listof Num)
;;
;;Examples:
(check-expect (duplicates '(1 1 2 1 2 2 3 3 4 4 4 5 7 7)) (list 1 2 3 4 7))
(check-expect (duplicates '(3 3 3 3 3 3 3)) (list 3))


(define (duplicates lon)

  (local
    [;;(how-many y ls) counts no of y in list(ls).
     ;;how-many: Num (listof Num) -> Nat
     (define (how-many y ls)
       (foldr (lambda (f cur) (cond [(equal? f y)(+ 1 cur)]
                      [else cur])) 0 ls))              
     ;;(mult-remover list) removes the repeated elements in list.
     ;;mult-remover: (listof Num) -> (listof Num)
     (define (mult-remover ls)
       (foldr (lambda (f cur)
                (cond [(member? f cur) cur]
                      [else (cons f cur)])) empty ls))]
     (mult-remover (filter (lambda (y) (> (how-many y lon) 1)) lon))))


;;Tests
(check-expect (duplicates empty) empty)
(check-expect (duplicates '(1 2 3 4 5 6 7 8 9 10)) empty)
(check-expect (duplicates '(1 2 2 3 4 3 2)) (list 3 2))
(check-expect (duplicates '(-1.01)) empty)
(check-expect (duplicates '(1/3 2/3)) empty)
(check-expect (duplicates '(1/3 1/3)) (list 1/3))
