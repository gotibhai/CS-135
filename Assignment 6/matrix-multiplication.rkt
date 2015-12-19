;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname matrix-multiplication) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
(define M (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define L (list (list 1 2 3) (list 2 3 4)))

(define (M-recursion matrix nat-num n)
  
  (cond [(empty? matrix) empty]
        [(= nat-num n) (first matrix)]
        [else (M-recursion (rest matrix) nat-num (add1 n))]))

(define (matrix-row matrix row)
  
  (M-recursion matrix row 0))

(define (matrix-col-recursion matrix col m)
  (cond [(empty? matrix) empty]
        [(= m col)
         (cons (first (first matrix)) (matrix-col-recursion (rest matrix) col m))]
        [else (matrix-col-recursion (m-list matrix) col (add1 m))]))

(define (matrix-col matrix col)
  
  (matrix-col-recursion matrix col 0))

(define (m-list list)
  
  (cond [(empty? list) empty]
        [else (cons (rest (first list)) (m-list (rest list)))]))

(define (matrix-size matrix1)
  (cond [(empty? matrix1) 0]
        [else (+ 1 (matrix-size (rest matrix1)))]))

(define (dot-product lon1 lon2)
  (cond [(empty? lon1) 0]
        [else (+ (* (first lon1) (first lon2))
                 (dot-product (rest lon1) (rest lon2)))]))

(define (matrix-multiply matrix1 matrix2 m n)
  (cond [(= m (matrix-size matrix1)) (list)]
        [(= n (- (matrix-size (first matrix2)) 1))
              (cons (dot-product(matrix-row matrix1 m) (matrix-col matrix2 n))
                    (matrix-multiply  matrix1 matrix2 (add1 m) (- n (- (matrix-size (first matrix2)) 1))))]
        [else (cons (dot-product(matrix-row matrix1 m) (matrix-col matrix2 n))
                    (matrix-multiply  matrix1 matrix2 m (add1 n)))]))

(matrix-multiply L M 0 0)

(define (l matrix size m)
  (cond [(= m 3) empty]
        [else (cons (first matrix) (l (rest matrix) size (add1 m)))]))

(define (strip-elements matrix size m)
  (cond [(= m size) matrix]
        [else (strip-elements (rest matrix) size (add1 m))]))

(define (new-list matrix size)
  (cond [(empty? (strip-elements matrix size 0)) (cons (l matrix size 0) empty)]
        [else (cons (l matrix size 0) (new-list (strip-elements matrix size 0) size))]))

(define (matrix-multiply-2 matrix1 matrix2)
  (new-list (matrix-multiply matrix1 matrix2 0 0) (matrix-size matrix2)))

(matrix-multiply-2 L M)






































;(define (matrix-formation list1 size m)
;  (cond [(empty? list1) empty]
;        [(= m size)  (matrix-formation  (list list1) size (- m size))]
;        [else (list (first list1) (matrix-formation (rest list1) size (add1 m)))]))

;(matrix-formation (matrix-multiply M M 0 0) (matrix-size M) 0)

;(define (list-recursion list1 nat-num n)
  
  ;(cond [(empty? list1) empty]
   ;     [(= nat-num n) (first list1)]
    ;    [else (list-recursion (rest list1) nat-num (add1 n))]))

;(define (my-list-ref list1 nat-num)
  
 ; (list-recursion list1 nat-num 0))

;(matrix-multiply M M 0 0)

;(define (new-list matrix m size)
 ; (cond [(= m size) empty]
  ;            [else (cons (my-list-ref matrix m) (new-list matrix (add1 m) size))])))

;(new-list (matrix-multiply M M 0 0) 0 (matrix-size M))
                      