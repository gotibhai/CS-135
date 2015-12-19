;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 10, Problem 1
;;***************************************************
;;

;;DEFINITIONS
;(define y 0)

;;(merge-sort lon func): sorts the list-lon using the mergesort alogirthm
;;
;; mergesort: (listof X) (X X -> Bool) -> (listof X)
;;
;; Examples:
(check-expect (mergesort (list 3 5 6 2 8 10 4.1) <) (list 2 3 4.1 5 6 8 10))
(check-expect (mergesort
               (list "willyboy" "carl" "andrew" "scissors" "wicky" "bob")
               string>?)
              (list
 "willyboy"
 "wicky"
 "scissors"
 "carl"
 "bob"
 "andrew"))

(define (mergesort lon func)
  (local [(define y 0)
;; USED FROM TUTORIAL SLIDES
          
;;(merge lon1 lon2 func) takes in two lists and a func and merges the lists
;; according to the function.
;;
;;merge: (listof Num) (listof Num) (X X -> Bool) -> (listof Num)
(define (merge lon1 lon2 func)  
(cond
[(and (empty? lon1) (cons? lon2)) lon2]
[(and (cons? lon1) (empty? lon2)) lon1]
[(and (cons? lon1) (cons? lon2))
(cond [(func (first lon1) (first lon2))
(cons (first lon1) (merge (rest lon1) lon2 func))]
[else (cons (first lon2) (merge lon1 (rest lon2) func))])]))
;;(div lon y mid func) takes in the list, and iterator y the mid of the
;; list and the func according to which you sort the function. 
;;
;;div: (listof X) Nat Nat (X X -> Bool)-> (listof X)

(define (div lon y mid func)
    (cond [(equal? func <) (cond [(< y mid)(cons (list-ref lon y)
                                        (div lon (add1 y) mid <))]
                                 [else empty])]
          [(equal? func >) (cond [(= y (length lon)) empty]
                                 [(>= y mid)(cons (list-ref lon y)
                                  (div lon (add1 y) mid >))])]))]

  (cond [(empty? lon) empty]
        [(= 1 (length lon)) lon]
        [else (local
           [(define mid (cond [(= 0 (remainder (length lon) 2)) (/ (length lon) 2)]
                               [else (/ (add1 (length lon)) 2)]))
            (define less (div lon y mid <))
            (define greater (div lon mid mid >))]
            (cond[(equal? func <) (merge (mergesort less func) (mergesort greater func) func)]
                 [else (merge (mergesort greater func) (mergesort less func) func)]))])))


;;Tests:
(check-expect (mergesort empty >) (quicksort empty >))
(check-expect (mergesort (list 3 5 2 1 7) >) (quicksort (list 3 5 2 1 7) >))
(check-expect (mergesort (list 3 5 2 1 7) <) (quicksort (list 3 5 2 1 7) <))
(check-expect (mergesort (list 0 1 0) <) (list 0 0 1))
(check-expect (mergesort (list 0) <) (list 0))
(check-expect (mergesort (list 0) >) (list 0))
(check-expect (mergesort (list 0 1 0) >) (list 1 0 0))
(check-expect (mergesort (list #\a #\c #\b #\d) char>?) (list #\d #\c #\b #\a))
(check-expect (mergesort (list #\a #\c #\b #\d) char<?) (list #\a #\b #\c #\d))
(check-expect (mergesort (list #\a) char>?) (list #\a))
(check-expect (mergesort (list #\d) char<?) (list #\d))
