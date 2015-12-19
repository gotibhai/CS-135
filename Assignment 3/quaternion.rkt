;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quaternion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 4
;;***************************************************
;;

(define-struct quaternion (cc ic jc kc))
;; A Quaternion is a (make-quaternion Num Num Num Num).

;; (quat-mult a b q1 q2) produces a new quaternion when
;; quaternions q1 and q2 are multiplied, given a and b, as defined.
;;
;; quat-mult: Num Num Quaternion Quaternion -> Quaternion
;;
;; Examples:
(check-expect (quat-mult 4 5
                         (make-quaternion 6 6 5 8)
                         (make-quaternion 1 10 11 12))
(make-quaternion -1399 206 39 96))

(check-expect (quat-mult 5 5 (make-quaternion 1 0 0 0)
                         (make-quaternion 1 0 7 8))
(make-quaternion 1 0 7 8))


(define (quat-mult a b q1 q2)
  (make-quaternion
   (+ (*(quaternion-cc q1) (quaternion-cc q2))
      (*(quaternion-ic q1) (quaternion-ic q2) a)
      (*(quaternion-jc q1) (quaternion-jc q2) b)
      (*(quaternion-kc q1) (quaternion-kc q2) a b -1))
   (+ (*(quaternion-cc q1) (quaternion-ic q2) )
      (*(quaternion-ic q1) (quaternion-cc q2) )
      (*(quaternion-jc q1) (quaternion-kc q2) b -1)
      (*(quaternion-kc q1) (quaternion-jc q2)  b))
   (+ (*(quaternion-cc q1) (quaternion-jc q2) )
      (*(quaternion-ic q1) (quaternion-kc q2) a)
      (*(quaternion-jc q1) (quaternion-cc q2))
      (*(quaternion-kc q1) (quaternion-ic q2) a -1))
   (+ (*(quaternion-cc q1) (quaternion-kc q2))
      (*(quaternion-ic q1) (quaternion-jc q2) )
      (*(quaternion-jc q1) (quaternion-ic q2) -1)
      (*(quaternion-kc q1) (quaternion-cc q2)))))


;;Tests
(check-expect (quat-mult 0 0 (make-quaternion 1 0 9 9)
                         (make-quaternion 1 0 5 8))
              (make-quaternion 1 0 14 17))
(check-expect (quat-mult 0 0 (make-quaternion 1 0 9 9)
                         (make-quaternion 1 0 5 8))
              (make-quaternion 1 0 14 17))
(check-expect (quat-mult 10 90 (make-quaternion 1 0 6 9)
                         (make-quaternion 1 0 4 8))
              (make-quaternion -62639 -1080 10 17))
              






