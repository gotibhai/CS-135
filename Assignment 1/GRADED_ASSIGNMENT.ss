ASSIGNMENT 01
Student's Quest ID: p2abbott

*******************************************************************************

**** Correctness Test Results *************************************************

53/58   Total Mark

 ** Question 1a: 9/9
 ** Question 1b: 6/6
 ** Question 1c: 4/4
 ** Question 2a: 8/8
 ** Question 2b: 8/8
 ** Question 2c: 6/6
 ** Question 3: 12/16
 ** Question 4: 0/1

(Question 1a, Test 001, 1 marks): Passed
(Question 1a, Test 002, 1 marks): Passed
(Question 1a, Test 003, 1 marks): Passed
(Question 1a, Test 004, 1 marks): Passed
(Question 1a, Test 005, 1 marks): Passed
(Question 1a, Test 006, 1 marks): Passed
(Question 1a, Test 007, 1 marks): Passed
(Question 1a, Test 008, 1 marks): Passed
(Question 1a, Test 009, 1 marks): Passed
(Question 1b, Test 001, 1 marks): Passed
(Question 1b, Test 002, 1 marks): Passed
(Question 1b, Test 003, 1 marks): Passed
(Question 1b, Test 004, 1 marks): Passed
(Question 1b, Test 005, 1 marks): Passed
(Question 1b, Test 006, 1 marks): Passed
(Question 1c, Test 001, 1 marks): Passed
(Question 1c, Test 002, 1 marks): Passed
(Question 1c, Test 003, 1 marks): Passed
(Question 1c, Test 004, 1 marks): Passed
(Question 2a, Test 001, 1 marks): Passed
(Question 2a, Test 002, 1 marks): Passed
(Question 2a, Test 003, 1 marks): Passed
(Question 2a, Test 004, 1 marks): Passed
(Question 2a, Test 005, 1 marks): Passed
(Question 2a, Test 006, 1 marks): Passed
(Question 2a, Test 007, 1 marks): Passed
(Question 2a, Test 008, 1 marks): Passed
(Question 2b, Test 001, 1 marks): Passed
(Question 2b, Test 002, 1 marks): Passed
(Question 2b, Test 003, 1 marks): Passed
(Question 2b, Test 004, 1 marks): Passed
(Question 2b, Test 005, 1 marks): Passed
(Question 2b, Test 006, 1 marks): Passed
(Question 2b, Test 007, 1 marks): Passed
(Question 2b, Test 008, 1 marks): Passed
(Question 2c, Test 001, 1 marks): Passed
(Question 2c, Test 002, 1 marks): Passed
(Question 2c, Test 003, 1 marks): Passed
(Question 2c, Test 004, 1 marks): Passed
(Question 2c, Test 005, 1 marks): Passed
(Question 2c, Test 006, 1 marks): Passed
(Question 3, Test final_001, 1 marks): Passed
(Question 3, Test final_002, 1 marks): Passed
(Question 3, Test final_003, 1 marks): Passed
(Question 3, Test final_004, 1 marks): FAILED; expected 175/9, saw 255/4
(Question 3, Test final_005, 1 marks): FAILED; expected 275/3, saw 77/2
(Question 3, Test final_006, 1 marks): FAILED; expected 100, saw 81/4
(Question 3, Test final_007, 1 marks): FAILED; expected 0, saw 319/4
(Question 3, Test sofar_001, 1 marks): Passed
(Question 3, Test sofar_002, 1 marks): Passed
(Question 3, Test sofar_003, 1 marks): Passed
(Question 3, Test sofar_004, 1 marks): Passed
(Question 3, Test sofar_005, 1 marks): Passed
(Question 3, Test sofar_006, 1 marks): Passed
(Question 3, Test sofar_007, 1 marks): Passed
(Question 3, Test sofar_008, 1 marks): Passed
(Question 3, Test sofar_009, 1 marks): Passed
(Question 4, Test 001, 1 marks): Checking participation.rkt: FAILED; expected
    #t, saw #f


**** functions.rkt *****************************************************************
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (doughnut-surface-area r z)
  (* 4 pi pi r z))

(define (future-value p r t)
  (* p (expt (+ 1 r) t)))

(define g 9.8)

(define (height v t)
  (- (* v t) (/ (* g t t) 2)))



**** conversion.rkt *****************************************************************
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define pound 453.59237)
#| ANNOTATION 1: Define some constants here (e.g. (define litres/gallon 3.78541)). |#
#| ANNOTATION 2: Define some constants here (e.g. (define litres/gallon 3.78541)). |#
#| ANNOTATION 3: Define constants for these. |#
(define (oz->gram oz)
  (*(/ oz 16) pound))

(define gallon 3785.41)
(define (ml->floz ml)
  ( * (/ ml gallon) 128))

(define (opo->gpl opo)
  (*(oz->gram opo) (/ 128 3.78541)))
#| END ANNOTATION 1 |#
#| END ANNOTATION 2 |#
#| END ANNOTATION 3 |#




**** grades.rkt *****************************************************************
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
#| ANNOTATION 4: Define constants for these. |#
#| ANNOTATION 5: Meaningful Parameter Names: Please use self explanatory names; comments explaining non-meaningful names are not sufficient |#
(define (cs135-grade-sofar 1mtg 2mtg pg ag)
  (* (+ (* 0.10 1mtg) (* 0.20 2mtg) (* 0.05 pg) (* 0.20 ag)) 20/11))

(define (cs135-final-exam sofar final)
  (+ (* 0.55 sofar) (* 0.45 final)))
#| END ANNOTATION 4 |#
#| END ANNOTATION 5 |#



**** participation.rkt *****************************************************************
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
#| ANNOTATION 6: Meaningful Parameter Names: Please use self explanatory names; comments explaining non-meaningful names are not sufficient |#
(define(k x)
  (* (/ 3 4) x))
#| END ANNOTATION 6 |#
#| ANNOTATION 9: Code Complexity: (* (/ 1 b) a) = (/ a b) |#

#| ANNOTATION 7: Excessive Line Length:
Break very long lines over multiple lines to improve readability. |#
#| ANNOTATION 8: Define constants:
Give special values like the weight for each course component a name with a constant definition.  For example, "(define final-wt .50)". The target grade, 50%, should be defined as a constant as well. |#
(define ( cs135-participation total-clicker-questions correct-questions wrong-questions)
     (* (/ (+ (* 2(min (k total-clicker-questions)  correct-questions)) (* 1 (min (- (k total-clicker-questions) correct-questions) wrong-questions ))) (* (k total-clicker-questions) 2)) 100))
#| END ANNOTATION 7 |#
#| END ANNOTATION 8 |#

#| END ANNOTATION 9 |#


**** End of graded assignment. *************************************************
