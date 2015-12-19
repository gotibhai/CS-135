;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 0, Bonus
;;***************************************************
;;


;; my-compose: (A -> B) (C -> A) -> (C -> B) 

(define (my-compose f g)
  (lambda (x) (f (g x))))


;; curry: (A B -> C) -> (A -> (B -> C))

(define (curry f)
  (lambda (c) (lambda (z) (f c z))))


;; uncurry: (A -> (B -> C)) -> (A B -> C)

(define (uncurry cur-fn)
  (lambda (a b)
    ((cur-fn a) b)))


;;eat-apples: (listof Sym) -> (listof Sym)

(define (eat-apples ls)
  (filter (my-compose not ((curry symbol=?) 'apple)) ls))


;;my-map: (A -> B) (listof A) -> (listof B)
(define (my-map func ls)    
  (foldr (uncurry (my-compose (curry cons) func)) empty ls))