;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;PART A------------------------------------------------

(define (intersection list1 list2)
  (foldr (lambda (x y)
            (cond [(member? x list2)
                   (cons x y)]
                  [else y])) empty list1))

;;PART B------------------------------------------------

(define (union list1 list2)
  (foldr (lambda (x y)
           (cond [(not(member? x list2))
                   (cons x y)]
                  [else y])) list2 list1))

;;PART C------------------------------------------------

(define (unique-fn list1 func)
  (foldr (lambda (x y)
           (cond [(func x y) y]
                 [else (cons x y)])) empty list1))
         
(unique-fn '(3 1 3) =)                         


           

