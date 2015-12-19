;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 06, Problem 3
;;***************************************************
;;


;; PART A -------------------------------------------------


;; A Matrix is a (listof (listof Num))
;; Requires: Length of all (listof Num) be the same

(define M (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define L (list (list 1 2 3)))
(define K (list (list 1 2 3)(list 2 3 4)))
(define Z (list (list 1.1 2.5 3.9)(list 2.5 3.5 4.20)(list 1.5 1.5 4.20)))

;;(M-recursion matrix nat-num n) returns the element of list at
;;    place nat-num! It uses accumulative recursion.
;;
;;M-recursion: (listof Num) Nat -> Num
;;
;;Examples:
(check-expect(M-recursion M 0 0) (list 1 2 3))
(check-expect(M-recursion (list (list 1 2 3) empty) 0 1) empty)


(define (M-recursion matrix nat-num n)
  
  (cond [(empty? matrix) empty]
        [(= nat-num n) (first matrix)]
        [else (M-recursion (rest matrix) nat-num (add1 n))]))

;;Tests:
(check-expect(M-recursion M 1 0) (list 4 5 6))
(check-expect(M-recursion empty 2 0) empty)


;;(matrix-row matrix row) returns the element of list (row) at
;;  row'th position using the helper function list-recursion.
;;
;; matrix-row: Matrix Nat -> (listof Num)
;; requires: matrix is non-empty
;;           row < (length matrix)
;;Examples:
(check-expect (matrix-row M 2) (list 7 8 9))
(check-expect (matrix-row M 0) (list 1 2 3))

(define (matrix-row matrix row)
  
  (M-recursion matrix row 0))

;;Tests:
(check-expect (matrix-row M 1) (list 4 5 6))
(check-expect (matrix-row Z 0) (list 1.1 2.5 3.9))
(check-expect (matrix-row L 0)(list 1 2 3))
(check-expect (matrix-row Z 1)(list 2.5 3.5 4.20))
(check-expect (matrix-row (list (list 1 2 3) empty) 1) empty)
(check-expect (matrix-row (list (list 1 2 3) empty) 0) (list 1 2 3))
(check-expect (matrix-row (list (list 1)) 0) (list 1))
(check-expect (matrix-row (list empty empty) 0) empty)

;; PART B -------------------------------------------------


;;(matrix-col-recursion matrix col m) recurses thorugh the matrix
;;    and returns the column at col'th position.
;;
;; matrix-col-recursion: Matrix Nat Nat -> (listof Num)
;; requires: matrix is non-empty
;;           col < (length (first matrix))
;;
;;Examples:
(check-expect (matrix-col-recursion M 0 0)(list 1 4 7))
(check-expect (matrix-col-recursion M 1 0)(list 2 5 8))

(define (matrix-col-recursion matrix col m)
  (cond [(empty? matrix) empty]
        [(= m col)
         (cons (first (first matrix)) (matrix-col-recursion (rest matrix) col m))]
        [else (matrix-col-recursion (m-list matrix) col (add1 m))]))

;;Tests:
(check-expect (matrix-col-recursion M 2 0)(list 3 6 9))
(check-expect (matrix-col-recursion L 0 0)(list 1))


;;(m-list list) takes in a Matrix and produces another Matrix
;;    after removing the first element from every row.
;;
;;m-list: Matrix -> Matrix
;;
;;Examples:
(check-expect (m-list(list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 2 3) (list 5 6) (list 8 9)))

(check-expect (m-list(list (list 2 3) (list 5 6) (list 8 9)))
              (list (list 3) (list 6) (list 9)))

(define (m-list list)
  
  (cond [(empty? list) empty]
        [else (cons (rest (first list)) (m-list (rest list)))]))

;;Tests:
(check-expect (m-list (list (list 2 3)))
              (list (list 3)))
(check-expect (m-list (list (list 1 2 3)))
              (list (list 2 3)))

;;(matrix-col matrix col) recurses thorugh the matrix and returns
;;   the coloumn at col'th number.
;;
;; matrix-col: Matrix Nat -> (listof Num)
;; requires: m is non-empty
;;           c < (length (first m))
;;
;;Examples:
(check-expect (matrix-col M 0)(list 1 4 7))
(check-expect (matrix-col M 2)(list 3 6 9))
              
(define (matrix-col matrix col)
  
  (matrix-col-recursion matrix col 0))

;;Tests:
(check-expect (matrix-col M 1)(list 2 5 8))
(check-expect (matrix-col L 0)(list 1))
(check-expect (matrix-col L 1)(list 2))
(check-expect (matrix-col L 2)(list 3))
(check-expect (matrix-col Z 0)(list 1.1 2.5 1.5))
(check-expect (matrix-col Z 1)(list 2.5 3.5 1.5))
(check-expect (matrix-col Z 2)(list 3.9 4.20 4.20))
(check-expect (matrix-col (list (list 1)) 0) (list 1))
(check-expect (matrix-col (list (list 1 2)) 0) (list 1))
(check-expect (matrix-col (list (list 1 2)) 1) (list 2))

;; PART C -------------------------------------------------

;;(get-element matrix row col) produces the element of the matrix
;;   at row'th row and col'th column using helper funciton matrix-row!
;;
;; get-element: Matrix Nat Nat -> Num
;; requires: matrix is non-empty
;;           row < (length matrix)
;;           col < (length (first matrix))
;;
;;Examples:
(check-expect (get-element M 0 2) 3)
(check-expect (get-element M 0 0) 1)

(define (get-element matrix row col)
  
  (matrix-row (matrix-row matrix row) col))

;;Tests:
(check-expect (get-element M 0 1) 2)
(check-expect (get-element M 1 2) 6)
(check-expect (get-element L 0 1) 2)
(check-expect (get-element L 0 2) 3)
(check-expect (get-element L 0 0) 1)
(check-expect (get-element Z 0 0) 1.1)
(check-expect (get-element Z 1 2) 4.20)
(check-expect (get-element Z 1 1) 3.5)

;; PART D -------------------------------------------------


;;(list-add list1 list2) takes in a 2 lists and produces a third list
;;    whose each element is the sum of the corrensponding
;;    elements of list1 and list2.
;;
;;list-add: (listof Num) (listof Num) -> (listof Num)
;; requires (length list1) = (length list2)
;;;
;;Examples:
(check-expect(list-add (list 1 2 3) (list 1 2 3)) (list 2 4 6))
(check-expect(list-add (list 1) (list 10)) (list 11))

(define (list-add list1 list2)
  
  (cond [(empty? list1) empty]
        [else (cons (+ (first list1) (first list2))
                    (list-add (rest list1) (rest list2)))]))

;;Tests:
(check-expect(list-add (list 1 2 3) (list 10 10 10)) (list 11 12 13))
(check-expect(list-add (list 1 2 3) (list 4 5 6)) (list 5 7 9))
(check-expect(list-add (list 0 0 0) (list 0 0 0)) (list 0 0 0))

;;((matrix-add  matrix1 matrix2) takes in 2 matrix's
;;   and returns a third matrix which is the sum of these 2 matrices.
;;  
;; matrix-add: Matrix Matrix -> Matrix
;; requires: (length matrix1) = (length matrix2)
;;           (length each row of matrix1) = (length each row of matrix2)
;;
;;Examples: 
(check-expect (matrix-add   (list (list 1 2 3) (list 4 5 6))
                            (list (list 1 2 3) (list 4 5 6)))
                            (list (list 2 4 6) (list 8 10 12)))

(check-expect (matrix-add   (list (list 1 2 3))
                            (list (list 1 2 3)))
                            (list (list 2 4 6)))

(define (matrix-add  matrix1 matrix2)
  
  (cond [(empty? matrix1)  empty]
        [else (cons(list-add (first matrix1) (first matrix2))
                   (matrix-add (rest matrix1) (rest matrix2)))]))

;;Tests: 
(check-expect (matrix-add   (list (list 1 2 3) (list 4 5 6))
                            (list (list 4 5 6) (list 1 2 3)))
                            (list (list 5 7 9) (list 5 7 9)))

(check-expect (matrix-add   (list (list 1))
                            (list (list 1)))
                            (list (list 2)))

(check-expect (matrix-add   (list (list 1 1 1 1 1 1))
                            (list (list 1 1 1 1 1 1)))
                            (list (list 2 2 2 2 2 2)))

(check-expect (matrix-add K Z) (list (list 2.1 4.5 6.9) (list 4.5 6.5 8.2)))

(check-expect (matrix-add   (list (list 1 0))
                            (list (list 1 1)))
                            (list (list 2 1)))

(check-expect (matrix-add   (list (list 1 2 3 4 5) (list 4 5 6 7 8))
                            (list (list 4 5 6 1 2) (list 1 2 3 9 10)))
                            (list (list 5 7 9 5 7) (list 5 7 9 16 18)))

(check-expect (matrix-add empty empty) empty)


;; PART E -------------------------------------------------


;;(matrix-size matrix1) takes in a matrix and returns it's size.
;;
;;matrix-size: Matrix -> Nat
;;
;;Examples:
(check-expect (matrix-size M) 3)
(check-expect (matrix-size L) 1)

(define (matrix-size matrix1)
  
  (cond [(empty? matrix1) 0]
        [else (+ 1 (matrix-size (rest matrix1)))]))

;;Tests:
(check-expect (matrix-size (list (list 1) (list 2))) 2)
(check-expect (matrix-size (list )) 0)

;;(dot-product l1 l2) takes in 2 lists l1 and l2 and computes
;;   the dot product of those two lists using recursion.
;;
;;dot-product: (listof Num) (listof Num) -> Num
;; requires: l1 and l2 are the same length
;;
;;Examples:
(check-expect (dot-product (list 1 1 1) (list 1 1 1)) 3)
(check-expect (dot-product (list 1 2 3) (list 1 2 3)) 14)

(define (dot-product l1 l2)
  
  (cond [(empty? l1) 0]
        [else (+ (* (first l1) (first l2))
                 (dot-product (rest l1) (rest l2)))]))

;;Tests:
(check-expect (dot-product (list 1 0 0) (list 1 50 50)) 1)
(check-expect (dot-product (list 0 10 0) (list 1 0 30)) 0)
(check-expect (dot-product (list 2) (list 1)) 2)
(check-expect (dot-product (list 2 2) (list 2 2)) 8)

;;(multiplying-matrix matrix1 matrix2 m n) takes in a 2 matrices
;;   and produces one list of all elements of the result of
;;   matrix multiplication.
;;
;;multiplying-matrix: Matrix Matrix Nat Nat ->(listof Num)
;;
;;Examples:
(check-expect(multiplying-matrix M M 0 0) (list 30 36 42 66 81 96 102 126 150))
(check-expect(multiplying-matrix K M 0 0) (list 30 36 42 42 51 60))

(define (multiplying-matrix matrix1 matrix2 m n)
  
  (cond [(= m (matrix-size matrix1)) (list)]
        
        [(= n (- (matrix-size (first matrix2)) 1))
              (cons (dot-product(matrix-row matrix1 m)
                                (matrix-col matrix2 n))
                    (multiplying-matrix  matrix1 matrix2     
                          (add1 m) (- n (- (matrix-size (first matrix2)) 1))))]
        
        [else (cons (dot-product(matrix-row matrix1 m) (matrix-col matrix2 n))
                    (multiplying-matrix  matrix1 matrix2 m (add1 n)))]))

;;Tests:
(check-expect(multiplying-matrix M M 0 0) (list 30 36 42 66 81 96 102 126 150))
(check-expect(multiplying-matrix L (list (list 1) (list 2) (list 3)) 0 0)
                                                               (list 14))

;;(part-lists matrix size m) takes in a list of all elements result of
;;   matrix multiplication and returns the list of size of
;;  coloumn of resultant matrix of first elements using m which is the counter.
;;
;; part-lists: (listof Num) Nat Nat -> (listof Num)
;;
;;Examples:
(check-expect(part-lists (list 1 2 3 4 5 6 7 8 9) 3 0) (list 1 2 3))
(check-expect(part-lists (list 1 2 3 4 5 6 7 8 9) 5 0) (list 1 2 3 4 5))

(define (part-lists matrix size m)
  
  (cond [(= m size) empty]
        [else (cons (first matrix)
                    (part-lists (rest matrix) size (add1 m)))]))

;;Tests:
(check-expect (part-lists (list 1 2 3) 1 0) (list 1))
(check-expect (part-lists (list 1 2 3) 3 0) (list 1 2 3))
(check-expect (part-lists (list 1 2 3 4) 0 0) empty)

;;(strip-elements matrix size m) takes in a list and returns
;;   the list after excluding the first x elements from the list
;;    where x is the size using m which is the counter. 
;;
;;strip-elements: (listof Num) Nat Nat -> (listof Num)
;;
;;Examples:
(check-expect (strip-elements (list 1 2 3 4 5 6) 2 0) (list 3 4 5 6))
(check-expect (strip-elements (list 1 2 3 4 5 6) 1 0) (list 2 3 4 5 6))


(define (strip-elements matrix size m)
  
  (cond [(= m size) matrix]
        [else (strip-elements (rest matrix) size (add1 m))]))

;;Tests:
(check-expect (strip-elements (list 1 2 3 4 5 6) 0 0) (list 1 2 3 4 5 6))
(check-expect (strip-elements (list 1 2 3 4 5 6) 5 0) (list 6))

;;(new-matrix list1 size) takes in a list and using helper funcitons
;;   strip-elements and part-lists produces a matrix from the list!
;;
;;new-matrix: (listof Num) Nat -> Matrix
;;
;;Examples:
(check-expect (new-matrix (list 1 2 3 4 5 6) 3)
              (list (list 1 2 3)(list 4 5 6)))

(check-expect (new-matrix (list 1 2 3 4 5 6) 2)
              (list (list 1 2) (list 3 4) (list 5 6)))

(define (new-matrix list1 size)
  
  (cond [(empty? (strip-elements list1 size 0))
         (cons (part-lists list1 size 0) empty)]
        [else (cons (part-lists list1 size 0)
                    (new-matrix (strip-elements list1 size 0) size))]))

;;Tests:
(check-expect (new-matrix (list 1 2 3 4 5 6 7 8) 4)
              (list (list 1 2 3 4)(list 5 6 7 8)))

(check-expect (new-matrix (list 1 2 3 4 5 6 7 8 9 10) 5)
              (list (list 1 2 3 4 5)(list 6 7 8 9 10)))

;;(matrix-multiply matrix1 matrix2) returns the result of
;;    matrix multiplication of matrix1 and matrix2!
;;
;; matrix-multiply: Matrix Matrix -> Matrix
;; requires: (length matrix1) = (length each row of matrix2)
;;           (length each row of matrix1) = (length matrix2)
;;
;;Examples:
(check-expect(matrix-multiply M M)
             (list (list 30 36 42) (list 66 81 96) (list 102 126 150)))

(check-expect(matrix-multiply K M)
             (list (list 30 36 42) (list 42 51 60)))

;;Definitions:
(define A (list (list 1)))
(define B (list (list 2)))
(define C (list (list 5 5) (list 1 1)))
(define H (list (list 1 0) (list 0 1)))
(define D (list (list 2 2 4) (list 1 1 0)))
(define E (list (list 1 2 4 5 7)))
(define F (list (list 1 2)))
(define G (list (list 0 0) (list 0 0)))
(define I (list (list 1 2 3) (list 0 4 5) (list 1 0 6)))
(define J (list (list 12/11 -6/11 -1/11)
                (list 5/22 3/22 -5/22)
                (list -2/11 1/11 2/11)))
(define X (list (list 1 0 0)
                (list 0 1 0)
                (list 0 0 1)))



(define (matrix-multiply matrix1 matrix2)
  
  (new-matrix (multiplying-matrix matrix1 matrix2 0 0)
              (matrix-size matrix2)))


;;Tests:
(check-expect (matrix-multiply A B) (list (list 2)))
(check-expect (matrix-multiply C D)
              (list (list 15 15) (list 20 3) (list 3 4)))
(check-expect (matrix-multiply A E)
              (list (list 1) (list 2) (list 4) (list 5) (list 7)))
(check-expect (matrix-multiply empty empty) (list empty))
(check-expect (matrix-multiply A A) (list (list 1)))
(check-expect (matrix-multiply K Z) (list (list 10.6 14 24.9) (list 15.7 21.5 37.2)))
(check-expect (matrix-multiply L M) (list (list 30 36 42)))
(check-expect (matrix-multiply A F) (list (list 1) (list 2)))
(check-expect (matrix-multiply F G) (list (list 0 0)))
(check-expect (matrix-multiply C C) (list (list 30 30) (list 6 6)))
(check-expect (matrix-multiply C H) C)
(check-expect (matrix-multiply (list (list 1 2) (list 3 4))
                               (list (list -2 1) (list 3/2 -1/2))) H)
(check-expect (matrix-multiply I J) X)
                               





























