;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname find-wpos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

;;(iter loc row x y) takes in row of chars and returns valid wpos's.
;;
;;iter: (listof Char) Nat Nat -> (listof WPos)
;;
;;Examples: See mutually recursive function find-wpos for Examples

(define (iter loc row x y)
  
  (cond [(and (> x 1)(empty? loc)) (cons (make-wpos row (- y x) true x) empty)]
        [(and (not(> x 1)) (empty? loc)) empty]
        [(equal? #\# (first loc)) (iter (rest loc) row (add1 x) (add1 y))]
        [(> x 1) (cons (make-wpos row  (- y x) true x) (iter (rest loc) row 0 (add1 y)))]
        [else (iter (rest loc) row 0 (add1 y))]))

;;Tests: See mutually recursive function find-wpos for Tests


;;find-wpos takes in row of chars and returns valid wpos's.
;;
;;find-wpos: (listof Char) Nat -> (listof WPos)
;;
;;Examples:
(check-expect (find-wpos (string->list "#") 0) empty)
(check-expect(find-wpos (string->list "") 0) empty)

(define (find-wpos loc row)
  (iter loc row 0 0))
                                         
                                               
(check-expect(find-wpos (string->list "..####..###.....##") 3)
             (list (make-wpos 3 2 #true 4)
                   (make-wpos 3 8 #true 3)
                   (make-wpos 3 16 #true 2)))
(check-expect (find-wpos (string->list "###") 0)
              (list (make-wpos 0 0 #true 3)))
(check-expect (find-wpos (string->list "##.##") 0)
              (list (make-wpos 0 0 #true 2) (make-wpos 0 3 #true 2)))
