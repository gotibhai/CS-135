;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 10, Problem 1
;;***************************************************
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz02 (read-puzzle "puzzle02.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define puzz05 (read-puzzle "puzzle05.txt"))
(define puzz06 (read-puzzle "puzzle06.txt"))
(define puzz07 (read-puzzle "puzzle07.txt"))
(define puzz08 (read-puzzle "puzzle08.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define puzz10 (read-puzzle "puzzle10.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-a '((#\A) (#\X)))
(define grid-b '((#\A #\B) (#\X #\Y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:

;; (transpose g) takes in a grid and produces it's transpose
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose empty) empty)

(define (transpose grid)
  
  (cond [(empty? grid) empty]
        [(empty? (first grid)) empty]
        [else (cons (map first grid) (transpose (map rest grid)))]))

;; Tests:
(check-expect (transpose grid-a)(list (list #\A #\X)))
(check-expect (transpose grid-b)(list (list #\A #\X)(list #\B #\Y)))
(check-expect (transpose (list (list #\A #\X)(list #\B #\Y)))grid-b)
         

;;(iter loc row x y) takes in row of chars and returns valid wpos's.
;;
;;iter: (listof Char) Nat Nat -> (listof WPos)
;;
;;Examples:
(check-expect (iter (string->list "#") 0 0 0 ) empty)
(check-expect(iter (string->list "") 0 0 0 ) empty)
(check-expect (iter (string->list "####") 0 0 0)
              (list (make-wpos 0 0 true 4)))

(define (iter loc row x y)
  
  (cond [(and (> x 1)(empty? loc)) (cons (make-wpos row (- y x) true x) empty)]
        [(and (not (> x 1)) (empty? loc)) empty]
        [(equal? #\# (first loc)) (iter (rest loc) row (add1 x) (add1 y))]
        [(> x 1) (cons (make-wpos row (- y x) true x) (iter (rest loc) row 0 (add1 y)))]
        [else (iter (rest loc) row 0 (add1 y))]))

;;Tests:
(check-expect (iter (string->list "###.###") 0 0 0 )
(list (make-wpos 0 0 #true 3) (make-wpos 0 4 #true 3)))
(check-expect (iter (string->list "...") 0 0 0) empty)
(check-expect (iter (string->list "#.#.#.#.#") 0 0 0)
              empty)

;; (find-wpos loc row)takes in row of chars and returns valid wpos's.
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "#") 0) empty)
(check-expect(find-wpos (string->list "") 0) empty)
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))

(define (find-wpos loc row)
  (iter loc row 0 0))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list ".....") 0) empty)
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
(check-expect(find-wpos (string->list "..####..###.....##") 3)
             (list (make-wpos 3 2 #true 4)
                   (make-wpos 3 8 #true 3)
                   (make-wpos 3 16 #true 2)))
(check-expect (find-wpos (string->list "###") 0)
              (list (make-wpos 0 0 #true 3)))
(check-expect (find-wpos (string->list "##.##") 0)
              (list (make-wpos 0 0 #true 2) (make-wpos 0 3 #true 2)))

;; the order does not matter: here is an example
;; that uses lists-equiv?

(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))true)

(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)

;;(all-wpos grid x) takes in a grid and return all valid wpos
;;   from all the grids.
;;
;;all-wpos: (listof (listof Char)) Nat -> (listof WPos)
;;
;;Examples:
(check-expect(all-wpos (list (list #\# #\# #\#))0)(list (make-wpos 0 0 #true 3)))
(check-expect(all-wpos (list (list #\# #\# #\#)(list #\# #\. #\# #\# #\. #\#))0)
             (list (make-wpos 0 0 #true 3) (make-wpos 1 2 #true 2)))

(define (all-wpos grid x)
  
  (cond [(= x (length grid)) empty]
        [else (append (find-wpos (list-ref grid x) x)
                      (all-wpos grid (add1 x)))]))

;;Tests:
(check-expect (all-wpos (list (list #\# #\# #\# #\# #\. #\.)(list #\. #\. #\. #\. #\. #\.))0)
              (list (make-wpos 0 0 #true 4)))
(check-expect (all-wpos (list (string->list "....") (string->list "....")) 0)
              empty)

;;(flip-all lswpos) flips all the wpos in lswpos
;;flip-all:  (listof WPos) -> (listof WPos)
;;Examples:
(check-expect (flip-all (list (make-wpos 1 6 #true 4)))
              (list (make-wpos 6 1 #false 4)))
(check-expect(flip-all (list (make-wpos 3 5 #true 6) (make-wpos 7 3 #true 4)))
(list (make-wpos 5 3 #false 6) (make-wpos 3 7 #false 4)))

(define (flip-all lswpos)
  
  (cond [(empty? lswpos) empty]
        [else (cons (flip (first lswpos)) (flip-all (rest lswpos)))]))

;;Tests:
(check-expect(flip-all (list (make-wpos 0 0 true 0)))
             (list (make-wpos 0 0 false 0)))
(check-expect(flip-all (list (make-wpos 7 3 #false 3) (make-wpos 3 6 #false 6)
 (make-wpos 0 8 #false 5))) (list (make-wpos 3 7 #true 3)
 (make-wpos 6 3 #true 6)
 (make-wpos 8 0 #true 5)))

;; (initial-state puzzle) produces the initial state of the puzzle in which
;;    we can start searching from.
;
;; initial-state: Puzzle -> State
;;
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))

(check-expect (initial-state puzz02)
              (make-state
 (list
  (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\.)
  (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\#)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\# #\# #\# #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\# #\. #\. #\# #\. #\. #\. #\.)
  (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))
 (list
  (make-wpos 1 6 #true 4)
  (make-wpos 3 5 #true 6)
  (make-wpos 7 3 #true 4)
  (make-wpos 9 0 #true 4)
  (make-wpos 7 3 #false 3)
  (make-wpos 3 6 #false 6)
  (make-wpos 0 8 #false 5))
 (list "ADAM" "ALBERT" "DAN" "DAVE" "JOHN" "KAREN" "LESLEY")))

(check-expect (initial-state puzz03)
              (make-state
 (list (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 #true 5)
  (make-wpos 1 0 #true 5)
  (make-wpos 2 0 #true 5)
  (make-wpos 3 0 #true 5)
  (make-wpos 4 0 #true 5)
  (make-wpos 0 0 #false 5)
  (make-wpos 0 1 #false 5)
  (make-wpos 0 2 #false 5)
  (make-wpos 0 3 #false 5)
  (make-wpos 0 4 #false 5))
 (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS")))

(define (initial-state puzz)
  
  (local [(define grid-c (map string->list (first puzz)))]
    (make-state grid-c (append (all-wpos grid-c 0)
                  (flip-all (all-wpos (transpose grid-c) 0)))(second puzz))))

;; Tests:
(check-expect(initial-state (list
 (list "...." "..##" "....")
 (list "AL")))(make-state (list (list #\. #\. #\. #\.)
                                (list #\. #\. #\# #\#)
                                (list #\. #\. #\. #\.))
            (list (make-wpos 1 2 #true 2)) (list "AL")))

(check-expect (initial-state (list
 (list "...." "...." "....")
 (list "AL")))
(make-state (list (list #\. #\. #\. #\.)
                  (list #\. #\. #\. #\.)
(list #\. #\. #\. #\.)) '() (list "AL")))


;;(prt grid len z) takes in a grid and prints len number of strings from the
;;    starting point.
;;
;;prt: (listof (listof Char)) Nat -> (listof Char)
;;
;;Examples:
(check-expect (prt (string->list "abcdefgh") 5)
(list #\a #\b #\c #\d #\e))
(check-expect (prt (string->list "abcdefgh") 1)
(list #\a))

(define (prt row len)

  (cond [(or (empty? row)(zero? len)) empty]
        [else (cons (first row) (prt (rest row) (- len 1)))]))

;;Tests:

(check-expect (prt (string->list "abcdefgh") 7)
(list #\a #\b #\c #\d #\e #\f #\g))

(check-expect (prt (string->list "abcdefgh") 8)
(list #\a #\b #\c #\d #\e #\f #\g #\h))


;;(iterate grid x y dest-x dest-y len) iterates throught the grid and
;;   returns the grid after reaching the first point of wpos
;;
;;iterate: (listof (listof Char)) Nat Nat Nat Nat Nat -> (listof (listof Char))
;;
;;Examples:
(check-expect (iterate  (list (list #\. #\. #\. #\.)
                              (list #\. #\. #\# #\#)
                              (list #\. #\. #\. #\.)) 0 0 0 0 4)
(list #\. #\. #\. #\.))
(check-expect (iterate  (list (list #\. #\. #\. #\.)
                                (list #\. #\. #\# #\#)
                                (list #\. #\. #\. #\.)) 0 0 1 2 1)
(list #\#))

(define (iterate grid x y dest-x dest-y len)
  
  (cond [(empty? grid) empty]
        [(= x dest-x) (cond [(= y dest-y) (prt (first grid) len)]
                            [else (iterate (map rest grid) x (add1 y) dest-x dest-y len)])]
        [else (iterate (rest grid) (add1 x) y dest-x dest-y len)]))      

;;Tests:
(check-expect (iterate  (list (list #\. )
                              (list #\. )
                              (list #\. )) 0 0 0 0 1)
(list #\.))

(check-expect (iterate  (list (list #\. #\. #\. #\.)
                                (list #\# #\# #\# #\#)
                                (list #\. #\. #\. #\.)) 0 0 1 0 4)
(list #\# #\# #\# #\#))

;; (extract-wpos g wp) consumes a Grid, a WPos and produces the (listof Char)
;;    corresponding to that word position within the Grid.
;;
;; extract-wpos: Grid WPos -> (listof Char)
;;
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
 (check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
 (check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))
 (check-expect  (extract-wpos grid-abc (make-wpos 0 1 true 2)) (list #\B #\C))

(define (extract-wpos g wp)
  
  (cond [(boolean=? true (wpos-horiz? wp))
        (iterate g 0 0 (wpos-row wp) (wpos-col wp) (wpos-len wp))]
        [else (iterate (transpose g) 0 0 (wpos-col wp) (wpos-row wp) (wpos-len wp))]))

;; Tests:  
(check-expect(extract-wpos (list (list #\. #\. #\. #\.)
                                (list #\# #\# #\# #\#)
                                (list #\. #\. #\. #\.)) (make-wpos 0 0 false 3))
(list #\. #\# #\.))
(check-expect(extract-wpos (list (list #\. #\. #\. #\.)
                                (list #\# #\# #\# #\#)
                                (list #\. #\. #\. #\.)) (make-wpos 1 0 true 4))
(list #\# #\# #\# #\#))
(check-expect(extract-wpos (list (list #\. #\. #\. #\.)
                                (list #\# #\# #\# #\#)
                                (list #\. #\. #\. #\.)) (make-wpos 1 0 true 2))
(list #\# #\#))

;;(swap row len z loc) swaps the loc with the char in the row
;;
;;swap: (listof Char) Nat Nat (listof Char) -> (listof Char)
;;
;;Examples: 
(check-expect (swap (string->list "abcdef") 3 3 (string->list "sss"))
(list #\s #\s #\s #\a #\b #\c #\d #\e #\f))

(check-expect (swap (string->list "abcdef") 3 0 (string->list "sss"))
(list #\s #\s #\s #\d #\e #\f))

(define (swap row len z loc)
  
  (cond [(= z len) (append loc row)]
        [else (swap (rest row) len (add1 z) loc)]))

;;Tests:
(check-expect (swap (string->list "abcdef") 3 0 (string->list "sss"))
(list #\s #\s #\s #\d #\e #\f))

(check-expect (swap (string->list "a") 1 0 (string->list "s"))
(list #\s))

;;(iterate-x grid x y dest-x dest-y len loc) iterates through the rows
;;   to find the right row and then call iterate-y.
;;
;;iterate-x: (listof (listof Char)) Nat Nat Nat Nat Nat (listof Char)
;;            -> (listof (listof Char))
;;Examples:
(check-expect(iterate-x (list (list #\. #\. #\. #\.)
                   (list #\# #\# #\# #\#)
                   (list #\. #\. #\. #\.)) 0 0 1 0 4 (string->list "sexy"))
(list (list #\. #\. #\. #\.) (list #\s #\e #\x #\y) (list #\. #\. #\. #\.)))

(check-expect(iterate-x (list (list #\# #\. #\. #\.)
                              (list #\# #\# #\. #\.)
(list #\# #\. #\. #\.)) 0 0 0 0 1 (string->list "s"))
(list (list #\s #\. #\. #\.) (list #\# #\# #\. #\.) (list #\# #\. #\. #\.)))


(define (iterate-x grid x y dest-x dest-y len loc)
  
(cond
  [(empty? grid) empty]
  [(= x dest-x) (cons (iterate-y (first grid) y dest-y len loc) (rest grid))]
  [else (cons (first grid) (iterate-x (rest grid) (add1 x) y dest-x dest-y len loc))]))

;;Tests:
(check-expect(iterate-x (list (list #\. #\. #\. #\.)
                   (list #\# #\# #\# #\#)
                   (list #\. #\. #\. #\.)) 0 0 1 0 2 (string->list "se"))
(list (list #\. #\. #\. #\.) (list #\s #\e #\# #\#) (list #\. #\. #\. #\.)))

(check-expect(iterate-x (list (list #\# #\. #\. #\.)
                              (list #\# #\# #\. #\.)
(list #\# #\. #\. #\.)) 0 0 0 0 1 (string->list "F"))
(list (list #\F #\. #\. #\.) (list #\# #\# #\. #\.) (list #\# #\. #\. #\.)))

(check-expect (iterate-x empty 0 0 1 0 2 (string->list "se")) empty)
             
;;(iterate-y row y dest-y len loc) iterates through the row
;;   and swaps the loc at dest-y. 
;;
;;iterate-y: (listof Char) Nat Nat Nat (listof Char)
;;            -> (listof Char)
;;Examples:
(check-expect (iterate-y (string->list "abcdef") 0 2 3 (string->list "sex"))
(list #\a #\b #\s #\e #\x #\f))

(check-expect (iterate-y (string->list "") 0 2 3 (string->list "sex"))
empty)


(define (iterate-y row y dest-y len loc)
  
  (cond
  [(empty? row) empty]
  [(= y dest-y) (swap row len 0 loc)]
  [else (cons (first row) (iterate-y (rest row) (add1 y) dest-y len loc))])) 

;;Tests:
(check-expect (iterate-y (string->list "abcdef") 0 0 3 (string->list "sex"))
(list #\s #\e #\x #\d #\e #\f))

(check-expect (iterate-y (string->list "abcdef") 0 0 1 (string->list "s"))
(list #\s #\b #\c #\d #\e #\f))

;; (replace-wpos g wp loc) produces a grid with the word position replaced by
;;   the word position replaced by the (listof Char).
;;
;; replace-wpos: Grid WPos (listof Char) -> Grid
;;requires: len in WPos is equal to length of (listof Char)
;;
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  
  (cond [(boolean=? true (wpos-horiz? wp)) (iterate-x g 0 0
                         (wpos-row wp) (wpos-col wp) (wpos-len wp) loc)]
        [else (transpose (iterate-x (transpose g) 0 0  (wpos-col wp) (wpos-row wp) (wpos-len wp) loc))]))
        
;; Tests:

(check-expect (replace-wpos grid-a (make-wpos 0 0 true 1) '(#\J))
(list (list #\J) (list #\X)))

(check-expect (replace-wpos grid-b (make-wpos 0 0 true 2) '(#\F #\U))
(list (list #\F #\U) (list #\X #\Y)))

(check-expect (replace-wpos grid-b (make-wpos 1 0 true 2) '(#\F #\U))
(list (list #\A #\B) (list #\F #\U)))

;; (fit? word cells) takes in a too lists of chars to see if the word can
;;    fit into the list of cells;
;;
;; fit? (listof Char) (listof Char) -> Bool
;;
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##")) false)

(define (fit? word cells)
  
  (cond [(empty? word) true]
        [(not(= (length word) (length cells))) false]
        [(or (char=? (first word) (first cells)) (char=? (first cells) #\#))
         (fit? (rest word) (rest cells))]
        [else false]))

;; Tests:
(check-expect (fit? (string->list "STARWARS") (string->list "########")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "##")) false)
(check-expect (fit? (string->list "S") (string->list "##")) false)
(check-expect (fit? (string->list "BOOO") (string->list "#OOO")) true)
(check-expect (fit? (string->list "BOOO") (string->list "BOOO")) true)


;;(ideal-wpos grid lswpos wp) takes in a lswpos (ls of all wpos's)and produces
;;   the most ideal wpos to use by parsing grid and using the wpos with the
;;   most number of filled chars using acc recursion with wp accumulator.
;;
;;ideal-wpos: (listof (listof Char)) (listof WPos) WPos -> WPos
;;
;;Examples:
(check-expect (ideal-wpos (list
  (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\# #\# #\# #\.)
  (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\#)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\# #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\# #\# #\# #\# #\. #\. #\. #\.)
  (list #\. #\. #\. #\# #\. #\. #\# #\. #\. #\. #\.)
  (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))
  (list
  (make-wpos 1 6 #true 4)
  (make-wpos 3 5 #true 6)
  (make-wpos 7 3 #true 4)
  (make-wpos 9 0 #true 4)
  (make-wpos 7 3 #false 3)
  (make-wpos 3 6 #false 6)
  (make-wpos 0 8 #false 5)) (make-wpos 1 6 #true 4))(make-wpos 1 6 #true 4))

(check-expect (ideal-wpos (list
   (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\A #\D #\A #\M #\.)
   (list #\. #\. #\. #\. #\. #\. #\. #\. #\# #\. #\.)
   (list #\. #\. #\. #\. #\. #\# #\# #\# #\# #\# #\#)
   (list #\. #\. #\. #\. #\. #\. #\# #\. #\# #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\# #\. #\. #\. #\.)
   (list #\. #\. #\. #\# #\# #\# #\# #\. #\. #\. #\.)
   (list #\. #\. #\. #\# #\. #\. #\# #\. #\. #\. #\.)
   (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))
   (list (make-wpos 3 5 #true 6) (make-wpos 7 3 #true 4)
         (make-wpos 9 0 #true 4) (make-wpos 7 3 #false 3)
         (make-wpos 3 6 #false 6) (make-wpos 0 8 #false 5))
         (make-wpos 3 5 #true 6))(make-wpos 0 8 #false 5))
   
(define (ideal-wpos grid lswpos wp)
  
  (cond [(empty? lswpos) wp]
        [(> (length(filter (λ(x) (not(char=? x #\#))) (extract-wpos grid (first lswpos))))
            (length(filter (λ(x) (not(char=? x #\#))) (extract-wpos grid wp))))
         (ideal-wpos grid (rest lswpos) (first lswpos))]
        [else (ideal-wpos grid (rest lswpos) wp)]))

;;Tests:
(check-expect (ideal-wpos(list (list #\A #\R #\E #\P #\O)
                               (list #\# #\# #\# #\# #\#)
                               (list #\# #\# #\# #\# #\#)
                               (list #\# #\# #\# #\# #\#)
                               (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 0 1 #false 5)
   (make-wpos 0 2 #false 5)
   (make-wpos 0 3 #false 5)
   (make-wpos 0 4 #false 5))(make-wpos 1 0 #true 5))(make-wpos 0 0 #false 5))

(check-expect (ideal-wpos
  (list (list #\A #\R #\E #\P #\O) (list #\R #\# #\# #\# #\#) (list #\E #\# #\# #\# #\#) (list #\P #\# #\# #\# #\#) (list #\O #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 1 #false 5)
   (make-wpos 0 2 #false 5)
   (make-wpos 0 3 #false 5)
   (make-wpos 0 4 #false 5))(make-wpos 1 0 #true 5))
              (make-wpos 1 0 #true 5))

;; (neigh s) takes in a parameters of a state s and produces a list of all
;;   next possible state using the above helper functions.
;;
;; neigh: (listof (listof Char)) (listof WPos) WPos (listof String)
;;        (listof String) -> (listof State)
;;
;; Examples:
(check-expect (neigh (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (ideal-wpos (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (make-wpos 0 0 true 3))
                          (list "CAT")(list "CAT"))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(check-expect (neigh (list (list #\# #\#) (list #\. #\.))
                     (list (make-wpos 0 0 #true 2))
                     (make-wpos 0 0 #true 2)
                     (list "SE" "CS") (list "SE" "CS"))
   (list (make-state (list (list #\S #\E) (list #\. #\.)) '() (list "CS"))
         (make-state (list (list #\C #\S) (list #\. #\.)) '() (list "SE"))))


(define (neigh grid pos ideal wordset words)
  
  (cond [(empty? words) empty]
        [(fit? (string->list(first words)) (extract-wpos grid ideal))
         (cons (make-state (replace-wpos grid ideal (string->list(first words))) (remove ideal pos) (remove (first words) wordset))
               (neigh grid pos ideal wordset (rest words)))]
        [else (neigh grid pos ideal wordset (rest words))]))

;;Tests:
(check-expect (neigh (list (list #\. #\. #\. #\#) (list #\. #\. #\. #\#))
                     (list (make-wpos 0 3 #false 2))(make-wpos 0 3 #false 2)
                     '()'())'())

(check-expect(neigh
 (list (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#)
       (list #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 #true 5)
  (make-wpos 1 0 #true 5)
  (make-wpos 2 0 #true 5)
  (make-wpos 3 0 #true 5)
  (make-wpos 4 0 #true 5)
  (make-wpos 0 0 #false 5)
  (make-wpos 1 0 #false 5)
  (make-wpos 2 0 #false 5)
  (make-wpos 3 0 #false 5)
  (make-wpos 4 0 #false 5))
 (ideal-wpos (list (list #\# #\# #\# #\# #\#)
                   (list #\# #\# #\# #\# #\#)
                   (list #\# #\# #\# #\# #\#)
                   (list #\# #\# #\# #\# #\#)
                   (list #\# #\# #\# #\# #\#))
             (list
  (make-wpos 0 0 #true 5)
  (make-wpos 1 0 #true 5)
  (make-wpos 2 0 #true 5)
  (make-wpos 3 0 #true 5)
  (make-wpos 4 0 #true 5)
  (make-wpos 0 0 #false 5)
  (make-wpos 1 0 #false 5)
  (make-wpos 2 0 #false 5)
  (make-wpos 3 0 #false 5)
  (make-wpos 4 0 #false 5))
             (make-wpos 0 0 #true 5))  
 (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS")
 (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
(list
 (make-state
  (list (list #\S #\A #\T #\O #\R) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\A #\R #\E #\P #\O) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\T #\E #\N #\E #\T) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\O #\P #\E #\R #\A) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\R #\O #\T #\A #\S) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "OPERA" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\S #\A #\T #\O #\R) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\A #\R #\E #\P #\O) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\T #\E #\N #\E #\T) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\O #\P #\E #\R #\A) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\R #\O #\T #\A #\S) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "OPERA" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))))


;; (neighbours s) takes in a state s and produces a list of all next possible
;;   state using the above helper functions.
;;
;; neighbours: State -> (listof State)
;;
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(check-expect (neighbours(initial-state(list (list "##" "..")(list "SE" "CS"))))
   (list (make-state (list (list #\S #\E) (list #\. #\.)) '() (list "CS"))
         (make-state (list (list #\C #\S) (list #\. #\.)) '() (list "SE"))))


(define (neighbours s)
  (neigh (state-grid s)(state-positions s)
         (ideal-wpos (state-grid s)(state-positions s)(first (state-positions s) ))
         (state-words s) (state-words s)))


;; Tests:
(check-expect (neighbours(initial-state (list (list "...#" "...#") empty)))
'())
(check-expect (neighbours(initial-state(list (list "..##""....") empty)))empty)
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR" )))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))
(check-expect(neighbours (make-state
 (list (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 #true 5)
  (make-wpos 1 0 #true 5)
  (make-wpos 2 0 #true 5)
  (make-wpos 3 0 #true 5)
  (make-wpos 4 0 #true 5)
  (make-wpos 0 0 #false 5)
  (make-wpos 1 0 #false 5)
  (make-wpos 2 0 #false 5)
  (make-wpos 3 0 #false 5)
  (make-wpos 4 0 #false 5))
 (list "SATOR" "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS")))
(list
 (make-state
  (list (list #\S #\A #\T #\O #\R) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\A #\R #\E #\P #\O) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\T #\E #\N #\E #\T) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\O #\P #\E #\R #\A) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\R #\O #\T #\A #\S) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "OPERA" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\S #\A #\T #\O #\R) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "AREPO" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\A #\R #\E #\P #\O) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "TENET" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\T #\E #\N #\E #\T) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "OPERA" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\O #\P #\E #\R #\A) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "ROTAS" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))
 (make-state
  (list (list #\R #\O #\T #\A #\S) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#) (list #\# #\# #\# #\# #\#))
  (list
   (make-wpos 1 0 #true 5)
   (make-wpos 2 0 #true 5)
   (make-wpos 3 0 #true 5)
   (make-wpos 4 0 #true 5)
   (make-wpos 0 0 #false 5)
   (make-wpos 1 0 #false 5)
   (make-wpos 2 0 #false 5)
   (make-wpos 3 0 #false 5)
   (make-wpos 4 0 #false 5))
  (list "SATOR" "AREPO" "TENET" "OPERA" "SATOR" "AREPO" "TENET" "OPERA" "ROTAS"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

 ;(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

 (time (disp (criss-cross puzz01)))
 ;(time (disp (criss-cross puzz02)))
 ;(time (disp (criss-cross puzz03)))
 ;(time (disp (criss-cross puzz04)))
 ;(time (disp (criss-cross puzz05)))
 ;(time (disp (criss-cross puzz06)))
 ;(time (disp (criss-cross puzz07)))
 ;(time (disp (criss-cross puzz08)))
 ;(time (disp (criss-cross puzz09)))
 ;(time (disp (criss-cross puzz10)))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

 