;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 06, Problem 1
;;***************************************************
;;

;; PART A -------------------------------------------------

;;Definitions
(define fib-const 0)
(define fib-const-2 1)

;;(fibonacci-slow num) produces the term of the fibonacci sequence
;;    at that (num) position.
;;
;;fibonacci-slow: Nat -> Nat
;;
;;Examples:
(check-expect (fibonacci-slow 0) 0)
(check-expect (fibonacci-slow 1) 1)

(define (fibonacci-slow num)
  
  (cond [(zero? num) fib-const]
        [(= 1 num) fib-const-2]
        [else (+ (fibonacci-slow (- num 1))
                 (fibonacci-slow (- num 2)))]))

;;Tests:
(check-expect (fibonacci-slow 2) 1)
(check-expect (fibonacci-slow 10) 55)
(check-expect (fibonacci-slow 13) 233)
(check-expect (fibonacci-slow 21) 10946)


;; PART B -------------------------------------------------


;;(fibonacci-acc m n f-1 f-2) calculates the term of the fibonacci
;;   sequence at the nth position using m as counter and f-1 and f-2
;;   as result of fib sequence (using accumulative recursion)!
;;requires m =< n except when n = 0 
;;
;;fibonacci-acc: Nat Nat Nat Nat -> Nat
;;
;;Examples:
(check-expect (fibonacci-acc 1 1 0 1) 1)
(check-expect (fibonacci-acc 1 0 0 1) 0)

(define (fibonacci-acc m n f-1 f-2)
  (cond [(= n 0) 0]
        [(= m n) (+ f-1 f-2)]
        [else (fibonacci-acc (add1 m) n (+ f-1 f-2) f-1)]))

;;Tests:
(check-expect (fibonacci-acc 1 10 0 1) 55)
(check-expect (fibonacci-acc 1 3 0 1) 2)
(check-expect (fibonacci-acc 1 19 0 1) 4181)


;;(fibonacci-fast num) calculates the term of the fibonacci
;;   sequence at the nth position by calling helper function fibonacci-acc!
;;
;;fibonacci-fast: Nat -> Nat
;;
;;Examples:
(check-expect (fibonacci-fast 10) 55)
(check-expect (fibonacci-fast 0) 0)

(define (fibonacci-fast num)
  
  (fibonacci-acc 1 num 0 1))

;;Tests:
(check-expect (fibonacci-fast 1) 1)
(check-expect (fibonacci-fast 2) 1)
(check-expect (fibonacci-fast 3) 2)
(check-expect (fibonacci-fast 19) 4181)


;; PART C -------------------------------------------------


;;(list-recursion list1 nat-num n) returns the element of list at
;;    place nat-num! It uses accumulative recursion.
;;
;;list-recursion: (listof Any) Nat Nat -> Num
;;requires nat-num >= n
;;         nat-num is < = num of elements in list 
;; 
;;Examples:
(check-expect (list-recursion '(a b c d e f) 0 0) 'a)
(check-expect (list-recursion '(c d e f) 3 0) 'f)
(check-expect (list-recursion '(1 2 3) 1 0) '2)

(define (list-recursion list1 nat-num n)
  
  (cond [(empty? list1) empty]
        [(= nat-num n) (first list1)]
        [else (list-recursion (rest list1) nat-num (add1 n))]))

;;Tests:
(check-expect (list-recursion empty 0 0) empty)
(check-expect (list-recursion (list 2 2 2 2) 3 0) 2)


;;(my-list-ref list1 nat-num) returns the element of list1 at
;;  nat-num'th position using the helper function list-recursion.
;;
;;my-list-ref: (listof Num) Nat -> Num
;;
;;Examples:
(check-expect(my-list-ref (list 2 4 5 6) 2) 5)
(check-expect(my-list-ref (list 1 4 5 6) 0) 1)

(define (my-list-ref list1 nat-num)
  
  (list-recursion list1 nat-num 0))

;;Tests:
(check-expect(my-list-ref empty 2) empty)
(check-expect(my-list-ref empty 0) empty)
(check-expect(my-list-ref (list 2) 0)(list-ref (list 2) 0))
(check-expect(my-list-ref (list 2 4) 1) (list-ref (list 2 4) 1))
(check-expect(my-list-ref (list 2 4) 4) empty)
(check-expect(my-list-ref (list 2 4 5 6) 3) (list-ref (list 2 4 5 6) 3))
(check-expect(my-list-ref (list 3 4 5 6 7 11) 5)
             (list-ref (list 3 4 5 6 7 11) 5))


;; PART D -------------------------------------------------


;;(string-recursion num char count) produces the list which consists
;;    of char num times using structural recursion.
;;
;;string-recursion: Nat Char Nat -> (listof Char)
;;
;;Examples:
(check-expect (string-recursion 1 #\p 1) (list #\p))
(check-expect (string-recursion 0 #\p 1) empty)
(check-expect (string-recursion 5 #\z 1) (list #\z #\z #\z #\z #\z))

(define (string-recursion num char count)
  
  (cond [(zero? num) empty]
        [(= num count) (cons char empty)]
        [else (cons char (string-recursion num char (add1 count)))]))

;;Tests:
(check-expect (string-recursion 2 #\a 1) (list #\a #\a))
(check-expect (string-recursion 3 #\x 1) (list #\x #\x #\x))


;;(string-of-char num char) takes in a num and produces a string
;;     of char num number of times using the helper
;;     function string-recursion.
;;
;;string-of-char: Nat Char -> Str
;;
;;Examples:
(check-expect(string-of-char 10 #\p)"pppppppppp")
(check-expect(string-of-char 1 #\p)"p")

(define(string-of-char num char)
  
  (list->string (string-recursion num char 1)))

;;Tests:
(check-expect(string-of-char 0 #\p) "")
(check-expect(string-of-char 11 #\p) "ppppppppppp")
(check-expect(string-of-char 3 #\q) "qqq")
(check-expect(string-of-char 2 #\a) "aa")
(check-expect(string-of-char 8 #\a) "aaaaaaaa")
(check-expect(string-of-char 20 #\a) "aaaaaaaaaaaaaaaaaaaa")


;; PART E -------------------------------------------------


;;(new-string list-string m) takes in a list of char and produces
;;   a new list after swapping vowels with x's. The number of x's depends
;;   on which vowel it is. for eg, 1st - 1 x, 2nd - 2x's etc..
;;
;;new-string: (listof Char) Nat -> (listof Char)
;;
;;Examples:
(check-expect (new-string (list #\a #\x #\x) 0)(list #\x #\x #\x))
(check-expect (new-string (list #\a #\b #\c) 0)(list #\x #\b #\c))

(define (new-string list-string m)
  
  (cond [(empty? list-string) empty]
        [(member? (first list-string) (list #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
        (append (string->list(string-of-char (add1 m) #\x))
               (new-string (rest list-string) (add1 m)))]
        [else (cons (first list-string)(new-string (rest list-string) m))]))

;;Tests:
(check-expect (new-string (list #\a) 0)(list #\x))
(check-expect (new-string (list #\a #\e #\i) 0)(list #\x #\x #\x #\x #\x #\x))
(check-expect (new-string (list #\h #\t) 0)(list #\h #\t))
(check-expect (new-string (list #\b #\y #\e) 0)(list #\b #\y #\x))
(check-expect (new-string (list #\x #\x #\x) 0)(list #\x #\x #\x))

;;(replace-vowels string) takes in a string and swaps all the vowels
;;     with x's corresponding to which #num of vowel it is.
;;
;;replace-vowels: Str -> Str
;;
;;Examples:
(check-expect(replace-vowels "helloworld")"hxllxxwxxxrld")
(check-expect(replace-vowels "a")"x")

(define (replace-vowels string)
  
  (list->string(new-string (string->list string) 0)))

;;Tests:
(check-expect(replace-vowels "ae")"xxx")
(check-expect(replace-vowels "abc")"xbc")
(check-expect(replace-vowels "aei")"xxxxxx")
(check-expect(replace-vowels "aeio")"xxxxxxxxxx")
(check-expect(replace-vowels "aeiou")"xxxxxxxxxxxxxxx")
(check-expect(replace-vowels "")"")
(check-expect(replace-vowels "x")"x")
(check-expect(replace-vowels "axe")"xxxx")
(check-expect(replace-vowels "axexixoxux")"xxxxxxxxxxxxxxxxxxxx")
(check-expect(replace-vowels "e")"x")
(check-expect(replace-vowels "i")"x")
(check-expect(replace-vowels "o")"x")
(check-expect(replace-vowels "u")"x")
(check-expect(replace-vowels "c")"c")
(check-expect(replace-vowels "hi")"hx")
(check-expect(replace-vowels "htr wq")"htr wq")
(check-expect(replace-vowels "htrwq")"htrwq")
(check-expect(replace-vowels "ctr e")"ctr x")
(check-expect(replace-vowels "A")"x")
(check-expect(replace-vowels "Abcx eEirfq")"xbcx xxxxxxxxxrfq")
(check-expect(replace-vowels "E")"x")
(check-expect(replace-vowels "I")"x")
(check-expect(replace-vowels "O")"x")
(check-expect(replace-vowels "U")"x")
(check-expect(replace-vowels "Aa")"xxx")
(check-expect(replace-vowels "jyhgA")"jyhgx")
(check-expect(replace-vowels "AEIOU")"xxxxxxxxxxxxxxx")
(check-expect(replace-vowels "muchagustro")"mxchxxgxxxstrxxxx")





















         


