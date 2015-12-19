;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define pound 453.59237)
(define (oz->gram oz)
  (*(/ oz 16) pound))

(define gallon 3785.41)
(define (ml->floz ml)
  ( * (/ ml gallon) 128))

(define (opo->gpl opo)
  (*(oz->gram opo) (/ 128 3.78541)))
