;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 09, Problem 1/Problem 2/Problem 3
;;***************************************************
;;

;;PART A------------------------------------------------

;;(intersection ls1 ls2) produces a list of elements which are in both
;; lists, ls1 and ls2.
;;
;;intersection: (listof Any) (listof Any) -> (listof Any)
;; requires: no duplicates in any list
;;
;;Examples:
(check-expect (intersection empty empty) empty)
(check-expect (intersection (list 1 2 3 4) '(lsd sex sex-again))  empty)
            


(define (intersection list1 list2)
  (foldr (lambda (x y)
            (cond [(member? x list2)
                   (cons x y)]
                  [else y])) empty list1))

;;Tests:
(check-expect (intersection (list 1 2 "ass") (list 39 8 7 "ass"))(list "ass"))
(check-expect (intersection (list 1) (list 3))empty)
(check-expect (intersection (list 1 2 3 4 5 7) (list 3 7 4 1 5 2))(list 1 2 3 4 5 7))
(check-expect (intersection (list " ") (list " " "ass")) (list " "))
(check-expect (intersection (list 1) '(blue "purple" green 2)) empty)
(check-expect (intersection (list 009) empty) empty)
(check-expect (intersection empty '("a" "b" "c"))empty)
(check-expect (intersection (list 009) empty) empty)

             

;;PART B------------------------------------------------


;;(union list1 list2) produces a list containing all elements
;; of ls1 and ls2 excluding duplicates.
;;
;;union: (listof Any) (listof Any) -> (listof Any)
;; requires: no duplicates in either list
;;
;;Examples:
(check-expect (union empty empty) empty)
(check-expect (union empty (list 10 20 "ui" 'a))(list 10 20 "ui" 'a))


(define (union list1 list2)
  
  (foldr (lambda (x y)
           (cond [(not(member? x list2))
                   (cons x y)]
                  [else y])) list2 list1))

;;Tests:
(check-expect (union (list 1) (list 2)) (list 1 2))
(check-expect (union (list 1 2 3 4 5 6 7) (list 4 2 1 3))(list 5 6 7 4 2 1 3))
(check-expect (union (list 1 2 "a" 'asdf) empty) (list 1 2 "a" 'asdf))
(check-expect (union (list 1 2 3) (list 4 2)) (list 1 3 4 2))
(check-expect (union (list 1 2 3 4) (list "yay" "wow" "haha" "lol" 'blah))
              (list 1 2 3 4 "yay" "wow" "haha" "lol" 'blah))
(check-expect (union (list 1 'w "e") (list "e"))(list 1 'w "e"))


             
;;PART C------------------------------------------------ 


;;(unique-fn list predicate) produces a list which removes
;; all duplicate elements according to the predicatefunction passed.
;;
;;unique-fn: (listof X) (X X-> Bool) -> (listof X)
;;
;;Examples:
(check-expect (unique-fn empty >) empty)
(check-expect (unique-fn empty <) empty)
(check-expect (unique-fn (list "as" 7) equal?) (list "as" 7))
(check-expect (unique-fn (list "fuck_my_life") string=?) (list "fuck_my_life"))
(check-expect (unique-fn (list "lafdo") string=?) (list "lafdo"))

(define (unique-fn ls1 func)
  
  (foldr (lambda (x1 y1)
           (cond [((lambda (z ls2)
                    (foldr (lambda (p  q)
                    (cond [(func z p) true]
                          [else  q ])) false ls2)) x1 y1) y1]               
                 [else (cons x1 y1)])) empty ls1))

;;Tests:
(check-expect (unique-fn (list 10 5 6 7 10) <=)  (list 10))
(check-expect (unique-fn (list ' X 'X #\b 172 8 'Y 'Y) equal?)
              (list 'X #\b 172 8 'Y))
(check-expect (unique-fn (list 7 7 7 7) =) (list 7))
(check-expect (unique-fn (list 1 1.05 2 1.2)
                         (lambda (x y) (> 0.1 (abs (- x y)))))
              (list 1.05 2 1.2))
(check-expect (unique-fn (list 1 2 9 7 13) <) (list 13))
(check-expect (unique-fn (list  8 7 6 4 2) <)
              (list  8 7 6 4 2))



;;PART D------------------------------------------------


;;(cross ls1 ls2) produes a list of all possible pairs
;; of elements in ls1 and ls2.
;;
;;cross: (listof Any) (listof Any) -> (listof (list Any Any))
;;
;;Examples:
(check-expect (cross empty (list 1 5 'fuck)) empty)
(check-expect (cross (list 'b 'lsd "sex") empty) empty)
(check-expect (cross (list 'b 'b) '(19 19))
              (list
 (list 'b 19)
 (list 'b 19)
 (list 'b 19)
 (list 'b 19)))

(define (cross ls1 ls2)
  (foldr (lambda (z a) (foldr cons a z)) empty 
(map (lambda (y) (map (lambda (x) (list x y)) ls1)) ls2)))

;;Tests:
(check-expect (cross (list 1 2 3 4)(list 9 5 6 7))
              (list
 (list 1 9)
 (list 2 9)
 (list 3 9)
 (list 4 9)
 (list 1 5)
 (list 2 5)
 (list 3 5)
 (list 4 5)
 (list 1 6)
 (list 2 6)
 (list 3 6)
 (list 4 6)
 (list 1 7)
 (list 2 7)
 (list 3 7)
 (list 4 7)))
(check-expect (cross '(100) '(a)) (list (list 100 'a)))
(check-expect (cross empty empty) empty)
(check-expect (cross (list "asss" 'ball 100) (list "ass" 'b 100))
              (list
 (list "asss" "ass")
 (list 'ball "ass")
 (list 100 "ass")
 (list "asss" 'b)
 (list 'ball 'b)
 (list 100 'b)
 (list "asss" 100)
 (list 'ball 100)
 (list 100 100)))


;;PART E------------------------------------------------


;;(jaccard ls1 ls2) computes the jaccard index of ls1 & ls2.
;;
;;jaccard: (listof Num) (listof Num) -> Num
;; requires: both ls1 and ls2 can't be empty
;;           (length ls1) =  (length ls2)
;;Examples:
(check-expect (jaccard empty (list "ass" 'b)) 0)
(check-expect (jaccard (list 1 2 3) (list 1 2 3)) 1)

(define (jaccard ls1 ls2)
  (/ (length (intersection ls1 ls2)) (length(union ls1 ls2))))

(check-expect (jaccard '(3 4 2 1) '(1 5 7 3)) 1/3)

;;Tests:
(check-expect (jaccard (list 10) empty) 0)
(check-expect (jaccard (list 100) (list 1)) 0)
(check-expect (jaccard (list 3 4 2 ) (list 1 5 17 3)) 1/6)

;;PART F------------------------------------------------

;;(take num list1) takes in a number and a list and produces
;;   only num number of elements of list1.
;;
;;take: Nat (listof Num) -> (listof Num)
;;
;;Examples:
(check-expect (take 0 (list 1 2 4 4 4 4 4)) empty)
(check-expect (take 1 (list 1 2 4 4 4 4 4)) (list 1))

         
(define (take num list1)

  (cond [(zero? num) empty]
        [(> num (length list1)) list1]
        [(> num 0) (cons (first list1) (take (sub1 num) (rest list1)))]))

;;Tests:
(check-expect (take 5 (list 1 2 4 4 4 4 4))(list 1 2 4 4 4))
(check-expect (take 1 empty) empty)
(check-expect (take 5 (list 2 3 4 6 7 8)) (list 2 3 4 6 7))
(check-expect (take 6 (list 2 3 4 6 7 8)) (list 2 3 4 6 7 8))
(check-expect (take 0 empty) empty)

;; QUESTION 3-----------------------------------------

;; A Feature Vector (FV) is a (listof Num)
;; A Document Identifier (DI) is a String
;; A Document Vector (DV) is a (list DI FV)
;; A Feature-Vector Association List (FV-AL) is
;; * empty
;; * (cons DV FV-AL)
;; require: each FV must be of the same length
;; each DV must be unique
;; A Document Pair Tuple (DPT) is a (list DI DI Num)
;; where Num corresponds to some similarity between
;; the feature vectors associated with each DI


;;PART A------------------------------------------------

;;(cmp-with-sim dv fv-al sim-m) makes a list of DPT by combining
;; each DI from fv-al and dv, and calculates the similarity acc to
;;  sim-m(similarity measure).
;; 
;;cmp-with-sim: DV FV-AL (((listof Num) (listof Num) -> Num) -> (listof DPT)
;;Examples:
(check-expect (cmp-with-sim empty empty jaccard) empty)

(define (cmp-with-sim dv fv-al sim-m)
  (map (lambda (z) (list (first dv) (first z)
                    (sim-m (second dv) (second z)))) fv-al))
  

;:Tests:
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4 5 6 ))
                            '(("t1" (1 2 3 4 5 6)) ("t2" (2 5 1 6))) jaccard)(list
 (list "t1" "t1" 1)
 (list "t1" "t2" 2/3)))
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4 5 6 ))
                            '(("t1" (1 2 3 4 5 6)) ("t2" (2 5 1 6))) jaccard)(list
 (list "t1" "t1" 1)
 (list "t1" "t2" 2/3)))
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4 5 6 ))
                            '(("t1" (1 2 3 4 5 6)) ("t2" (2 5 1 6))) jaccard)(list
 (list "t1" "t1" 1)
 (list "t1" "t2" 2/3)))

;;PART B------------------------------------------------

;; (find-all-exact fv-al sim-m) takes in a FV-AL and sim-m(similarity
;;   measure) and makes a list of DPT's that correspond to all ordered pairs
;;   of DI's in the fv-al that have similarity-measure = 1.
;;
;; find-all-exact: FV-AL ((listof Num) (listof Num) -> Num) -> (listof DPT)
;; requires: DPT's with identical DI's shouldn't be produced as part of the list
;;
;; Examples:


(define (find-all-exact given-fv-al sim-m)
  (foldr (lambda (a b) (foldr (lambda (c d)
  (cond [(= (sim-m (second c) (second a)) 1)
  (cond [(string=? (first c) (first a)) d]
        [else (cons (list (first c) (first d) 1) d)])])) b given-fv-al))
        empty given-fv-al))

;;Tests:
(check-expect (find-all-exact (list (list "t5" (list 12))) jaccard) empty)

;;PART C------------------------------------------------

;;(redundant? dpt-1 dpt-2) takes in 2 DPT's and determines the equivalence of
;;  the set of DI's and produces a boolean value acc to that.
;;  
;; redundant?: DPT DPT -> Bool
;;
;; Examples:
(check-expect (redundant? '("t9" "t9" 1) '("t9" "t9" 1)) true)
(check-expect (redundant? '("t5" "t5" 1) '("t5" "t5" 0)) true)


(define (redundant? dpt-1 dpt-2)
  
  (or (and (string=? (first dpt-1) (first dpt-2))
           (string=? (second dpt-1) (second dpt-2)))
      (and (string=? (first dpt-1) (second dpt-2))
          (string=? (first dpt-2)  (second dpt-1)))))
  
           
;;Tests:
(check-expect (redundant? (list "t1" "t8" 1)(list "t8" "t1" 0)) true)
(check-expect (redundant? (list "t5" "t1" 1) (list "t4" "t6" 1)) false)
(check-expect (redundant? (list "t5" "t1" 1) (list "t5" "t6" 1)) false)
(check-expect (redundant? (list "t5" "t1" 1) (list "t5" "t1" 1)) true)


;;PART D
;;
;;(find-similar-within-d given-fv-al d sim-m) takes in a FV-AL(given-fv-al), a
;;   threshold num(d) and a sim-m (similarity-measure) and output's
;;   the DPT's of all combinations of pairs in the fv-al whose sim-m is threshold.
;;   
;;find-similar-within-d: FV-AL Num ((listof Num)(listof Num)->Num)->(listof DPT)
;;                        
;; requires:
;; shouldn't return DPT's with redundant DI's
;; 0 < d <1 and k > 0
;; 
;; Examples:
(check-expect (find-similar-within-d empty 1 jaccard) empty)

(define (find-similar-within-d given-fv-al d sim-m)
  
  (filter (lambda (x)
  (< d (third x))) (filter (lambda (y)
                  (not (string=? (first y) (second y))))
                (unique-fn (foldr (lambda (p q)
                (append (cmp-with-sim p given-fv-al sim-m) q))empty given-fv-al)
                                                                    redundant?))))
                                                     
;;Tests:
(check-expect (find-similar-within-d (list (list "x" (list 4 3 2 1))
                                           (list "y" (list 1 2 3 4))) 0.5
                                                                      jaccard)
              (list (list "y" "x" 1)))



;;PART E
;; (find-k-similar-within-d given-fv-al d k sim-m) takes in a FV-AL,
;;   a threshold num(d), +ve num k and sim-m (similarity-measure) and
;;   outputs the list of DPT's of k similar pairs of FV in the given-fv-al
;;   whose sim-m is above d.
;;   
;; find-k-similar-within-d: FV-AL Num Nat ((listof Num)(listof Num)->Num) ->
;;                          (listof DPT)
;;requires: no identical or redundant DPT's in the list
;; 0 < d < 1 and k > 0
;; 
;; Examples:

(define (find-k-similar-within-d given-fv-al d k sim-m)
  (take (quicksort (find-similar-within-d given-fv-al d sim-m)
                   (lambda (a b)
                   (> (third a) (third b)))) k))


;;Tests:
   









 
