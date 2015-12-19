#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; puzlib :: A module to provide useful helper functions for 
;;           CS 135, Fall 2015, Assignment 10 
;;           by Dave Tompkins [dtompkins]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide read-puzzle solve disp lists-equiv? unused-cell empty-cell)

;; read-puzzle: read in a puzzle file
;; solve: performs the main search
;; disp: display a list of strings in the interactions window
;; lists-equiv?: useful for testing
;; unused-cell, empty-cell: constants for grids & puzzle files


;; constants for grids & puzzle files:
(define comment-char #\;)
(define unused-cell #\.)
(define empty-cell #\#)


;; (grid-char? c) determines if c is a valid grid character
;; grid-char: Char -> Bool

(define (grid-char? c)
  (or (char=? c unused-cell) (char=? c empty-cell)))


;; (next-line stream type) reads in a line from the file stream,
;;   verifying the line type ('numeric, 'grid, 'word)
;;   ignoring any lines starting with a semi-colon (;)
;; next-line: 'FileStream' Sym -> Str

(define (next-line stream type)
  (local [(define line (read-line stream))]
    (cond [(eof-object? line)
           (error "error: reached end of file")]
          [(char=? (string-ref line 0) comment-char) (next-line stream type)]
          [(and (symbol=? type 'numeric)
                (not (string->number line)))
           (error "error: invalid numeric input in file")]
          [(and (symbol=? type 'grid)
                (not (andmap grid-char? (string->list line))))
           (error "error: invalid grid characters in file")]
          [(and (symbol=? type 'word)
                (not (andmap char-alphabetic? (string->list line))))
           (error "error: invalid word characters in file")]          
          [else line])))


;; (read-puzzle filename) reads in the puzzle contained in filename
;;   and produces a list with two elements:
;;   * the puzzle grid (a listof str) [with only .'s and #'s]
;;   * the list of words
;; read-puzzle Str -> (list (listof Str) (listof Str))

(define (read-puzzle filename)
  (local [(define in (open-input-file filename))
          (define numlines (string->number (next-line in 'numeric)))
          (define grid (build-list numlines (λ (n) (next-line in 'grid))))
          (define numwords (string->number (next-line in 'numeric)))
          (define words (build-list numwords (λ (n) (next-line in 'words))))]
    (list grid words)))


;; Note that in the following contracts, X is your State

;; (solve initial-state neighbours solved?) solves the problem by searching
;;   from an initial-state until a solution is found (solved? is true)
;;   or false if no solution exists.
;;   neighbours produces a list of legal next states from a given state.
;; find-route: X (X -> (listof X)) (X -> Bool) -> (anyof false X)

(define (solve initial-state neighbours solved?)
  (local
    [;; (solve/single state) searches outward from state 
     ;;   looking for a solution
     ;; solve/single: X -> (anyof false X)
     (define (solve/single state)
       (cond [(solved? state) state]
             [else (solve/list (neighbours state))]))
     
     ;; (solve/list lostate) searches from every state in lostate
     ;;   looking for a solution
     ;; solve/list: (listof X) -> (anyof false X)
     (define (solve/list lostate)
       (cond [(empty? lostate) false]
             [else (local [(define fresult (solve/single (first lostate)))]
                     (cond [(false? fresult) (solve/list (rest lostate))]
                           [else fresult]))]))]
    
    (solve/single initial-state)))


;; (disp los) displays a list of strings los in the interactions window
;;   replacing periods (.) with spaces
;; disp: (listof Str) -> Void
;; effects: displays result in the interactions window

(define (disp los)
  (local [(define los/lf (map (λ (s) (string-append s "\n")) los))
          (define loc (apply append (map string->list los/lf)))
          (define clean (map (λ (c) (cond [(char=? unused-cell c) #\space]
                                          [else c])) loc))]
    (display (list->string clean))))


;; lists-equiv? is not really part of this assignment,
;; but generally useful in tests where we don't care about ordering.
;; The approach is a bit sneaky, but very succinct: Check that
;; every element of l1 appears somewhere in l2 (in terms of equal?),
;; and that every elements of l2 appears somewhere in l1.

;; (lists-equiv? l1 l2) determines whether the l1 and l2 are "equivalent"
;;   and are essentially the same up to reordering.
;; lists-equiv?: (listof Any) (listof Any) -> Bool
;; requires: elements of l1 (or l2) are unique (no duplicates)

(define (lists-equiv? l1 l2)
  (and (= (length l1) (length l2))
       (andmap (λ (x1) (ormap (λ (x2) (equal? x1 x2)) l2)) l1)
       (andmap (λ (x2) (ormap (λ (x1) (equal? x1 x2)) l1)) l2)))
