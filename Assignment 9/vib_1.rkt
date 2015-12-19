;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname vib_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;(intersection list1 list2) produces a list of elements shared by both
;; lists, list1 and list2.
;;intersection: (listof Any) (listof Any) -> (listof Any)
;; requires: no duplicates in either list
;;Examples:
(check-expect (intersection '("as" 2 4) '(5 6 "as"))
              '("as"))
(check-expect (intersection '( 1 2 3 4 5) '(3))
              '(3))
(check-expect (intersection '(1 2 3 4) '(asd we blah))
              empty)

(define (intersection list1 list2)
  (filter (λ (x) (member? x list1)) list2))

;;Tests:
(check-expect (intersection '(1 2 3 4 5) '(3 4 1 5 2))
              '(3 4 1 5 2))
(check-expect (intersection '("") '("" "a")) '(""))
(check-expect (intersection empty '(blue "red" green 2)) empty)
(check-expect (intersection '(red "green" blue 007) empty) empty)
(check-expect (intersection empty empty) empty)
(check-expect (intersection '("a" "b" "c") '("a" "b" "c"))
              '("a" "b" "c"))


;;PART B



;;(union list1 list2) produces a list containing all elements
;; of list1 and list2, without duplicates.
;; union: (listof Any) (listof Any) -> (listof Any)
;; requires: no duplicates in either list
;;Examples:
(check-expect (union empty empty) empty)
(check-expect (union '(1 2 3 w "e") '(4 1 2 3 "e"))
              (list 'w 4 1 2 3 "e"))

(define (union list1 list2)
  (foldr cons (intersection list1 list2)
         (foldr cons (filter (λ (x) (not (member? x list1))) list2)
                (filter (λ (x) (not (member? x list2))) list1))))

;;Tests:
(check-expect (union empty '(1 2 "a" asdf))
              '(1 2 "a" asdf))
(check-expect (union '(1) '(2)) '(1 2))
(check-expect (union '("a" "b" "c" "d") '("a" "b" "c" "d"))
              '("a" "b" "c" "d"))
(check-expect (union '(1 2 "a" asdf) empty)
              '(1 2 "a" asdf))
(check-expect (union '(1 2 3 4) '(4 2))
              '(1 3 4 2))
(check-expect (union '(1 2 3) '("As" "XXXX" "YYY" "U" blah))
              '(1 2 3 "As" "XXXX" "YYY" "U" blah))






; ;;for implementing it like mul-reomv-2 function,
; ;we need a new predicate function (instead of member?)
; (define (new-member? x list predicate) 
;   (cond
;     [(empty? list) false]
;     [(predicate x (first list)) true]
;     [else (new-member? x (rest list) predicate)]))
; 
; (define (new-member? t list predicate)
;   (foldr (λ(a b) (cond
;                    [(predicate t a) true]
;                    [else b])) false list))
; ;;inside (unique-fn given-list pred)
; (λ (t) (foldr (λ(a b) (cond
;                    [(predicate t a) true]
;                    [else b])) false list))
; 
;     
;     
;   
; (define (unique-fn given-list predicate)
;   (foldr (λ (x y)
;            (cond
;              [(new-member? x y) y]
;              [else (cons x y)])) given-list))
; ==
; (define (unique-fn-2 given-list predicate)
;   (foldr (λ (x y)
;            (cond
;              [((λ (t) (foldr (λ(a b) (cond
;                    [(predicate t a) true]
;                    [else b])) false given-list)) x y) y]
;              [else (cons x y)])) empty given-list))
;   
;     




;;PART C


;;(unique-fn list predicate) produces a version of list in which
;; all duplicate elements, according to predicate, are removed.
;;unique-fn: (listof X) (X X-> Bool) -> (listof X)
;;Examples:
(check-expect (unique-fn empty >) empty)
(check-expect (unique-fn '("ad") string=?) '("ad"))
(check-expect (unique-fn '(1 2 3 0) >)
              '(1 0))

(define (unique-fn given-list predicate)
  (foldr (λ (x y)
           (cond   [((λ (t list)
                    (foldr (λ(a b)
                    (cond [(predicate t a) true]
                          [else b])) false list)) x y) y]
             [else (cons x y)])) empty given-list))

;;Tests:
(check-expect (unique-fn '(1 1.05 2 1.2)
                         (λ (x y) (> 0.1 (abs (- x y)))))
              '(1 2 1.2))
(check-expect (unique-fn '(10 5 6 7 10) <=)
              '(10))
(check-expect (unique-fn '(10 5 6 7 10) >)
              '(10 5 6 7 10))
(check-expect (unique-fn '("a" "b" "a " "DXe" "a" "B" "a") string=?)
              '("a" "b" "a " "DXe" "B"))


;;PART D

;;(cross list1 list2) produes a list of all possible pairs
;; of elements in list1 and list2.
;;cross: (listof Any) (listof Any) -> (listof (list Any Any)
;;Examples:
(check-expect (cross empty '(2 4 dsf)) empty)
(check-expect (cross '(a asd "as") empty) empty)
(check-expect (cross '(a a) '(1 1))
              '((1 a) (1 a) (1 a) (1 a)))

(define (cross list1 list2)
  (foldr (λ (x r) (foldr cons r x)) empty
         (map (λ (x) (map (λ (y) (list x y)) list1)) list2)))

;;Tests:

(check-expect (cross '(3 2) '(1 2 3 4))
              '((1 3) (1 2) (2 3) (2 2) (3 3) (3 2) (4 3) (4 2)))
(check-expect (cross '(1) '(a)) '((a 1)))
(check-expect (cross empty empty) empty)
(check-expect (cross '("a" b 1) '("a" b 1))
              '(("a" "a") ("a" b) ("a" 1) (b "a")
                          (b b) (b 1) (1 "a") (1 b) (1 1)))




;;PART E

;;(jaccard lon1 lon2) computes the jaccard index of lon1 and lon2.
;;jaccard: (listof Num) (listof Num) -> Num
;; requires: both lon1 and lon2 can't be empty
;;           lon1 and lon2 have same length.
;;Examples:
(check-expect (jaccard '(1) empty) 0)
(check-expect (jaccard '(1) '(1)) 1)
(check-expect (jaccard '(3 4 2 1) '(1 5 7 3)) 1/3)

(define (jaccard lon1 lon2)
  (/ (length(intersection lon1 lon2))
     (length (union lon1 lon2))))

;;Tests:
(check-expect (jaccard '(op p) '("afdadf" op b)) 1/4)
(check-expect (jaccard empty '("afdadf" b)) 0)
(check-expect (jaccard '(1 2 3) '(1 2 3)) 1)


