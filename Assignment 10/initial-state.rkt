;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname initial-state) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "puzlib.rkt")

(define puzz02 (read-puzzle "puzzle02.txt"))

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

(define (first-ele grid)
  
  (cond [(empty? grid) empty]
        [else(cons (first (first grid)) (first-ele (rest grid)))]))

;;Tests: Please see transpose for mutually recursive examples

;;(strip-1 grid) takes in a list and strips the first element of all lists
;;   and returns the new list.
;;
;;strip-1: (listof (listof char)) -> (listof (listof char))
;;
;;Examples:Please see mutually recursive function transpose

(define (strip-1 grid)
  (map rest grid))

;;Tests:Please see mutually recursive function transpose

(define (transpose grid)
  
  (cond [(empty? grid) empty]
         [(empty? (first grid)) empty]
        [else (cons (first-ele grid) (transpose (strip-1 grid)))]))

(define (iter loc row x y)
  
  (cond [(and (> x 1)(empty? loc)) (cons (make-wpos row (- y x) true x) empty)]
        [(and (not(> x 1)) (empty? loc)) empty]
        [(equal? #\# (first loc)) (iter (rest loc) row (add1 x) (add1 y))]
        [(> x 1) (cons (make-wpos row (- y x) true x) (iter (rest loc) row 0 (add1 y)))]
        [else (iter (rest loc) row 0 (add1 y))]))

(define (find-wpos loc row)
  (iter loc row 0 0))

(define (all-wpos grid x)
  (cond [(= x (length grid)) empty]
        [else (append (find-wpos (list-ref grid x) x)
                      (all-wpos grid (add1 x)))]))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))



(define (initial-state puzz)
  (local [(define grid-c (map string->list (first puzz)))]
    (make-state grid-c (append (all-wpos grid-c 0)
          (flip-all(all-wpos (transpose grid-c) 0))) (second puzz)))) 


(initial-state puzz02)