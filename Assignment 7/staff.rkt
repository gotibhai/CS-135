;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname staff) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 07, Problem 1
;;***************************************************
;;

;; A Department-List is a (listof Str)
(define-struct staff-member (id name dept))
;; A Staff-Member is a (make-staff-member Nat Str Str)
;; requires: id is unique
;; (i.e., every staff-member with the same id also has the same name)
(define-struct salary (staff-id base bonus)) ;; A Salary is a (make-salary Nat Num Num) ;; requires: base, bonus ≥ 0
;; A Staff-List is a (listof Staff-Member)
;; requires: elements are sorted by
;; A Salary-List is a (listof Salary)

;;DEFINITIONS

(define staff1 (make-staff-member 1 "John" "Engineering" ))
(define staff2 (make-staff-member 2 "Adam" "R&D" ))
(define staff3 (make-staff-member 3 "Albert" "Engineering" ))
(define staff4 (make-staff-member 4 "Liz" "Finance" ))
(define staff5 (make-staff-member 5 "Anne" "Defence" ))
(define staff6 (make-staff-member 6 "Suzy" "R&D" ))
(define staff7 (make-staff-member 7 "Leslie" "R&D" ))
(define staff8 (make-staff-member 8 "Josh" "Engineering" ))
(define staff9 (make-staff-member 9 "Apple" "Engineering" ))

(define salary1 (make-salary 1 50000 10))
(define salary2 (make-salary 2 74000 250))
(define salary3 (make-salary 3 85000 10))
(define salary4 (make-salary 4 60000 0))
(define salary5 (make-salary 5 93000 100))
(define salary6 (make-salary 6 68500 0))
(define salary7 (make-salary 7 250000 0))
(define salary8 (make-salary 8 120000 0))

(define staff-list (list staff1 staff2 staff3 staff4 staff5 staff6 staff7 staff8))
(define sal-list (list salary1 salary2 salary3 salary4 salary7 salary8))
(define dept-list (list "Engineering" "Defence" "R&D" "Management"))

(define (exists? staff-mem staff-list)
  (cond [(empty? staff-list) false]
        [(= (staff-member-id staff-mem) (staff-member-id (first staff-list))) true]
        [else (exists? staff-mem (rest staff-list))]))

(define (add-staff staff-list staff-mem )
  (cond [(empty? staff-list) (list staff-mem)]
        [(exists? staff-mem staff-list) staff-list]
        [(< (staff-member-id staff-mem) (staff-member-id (first staff-list))) (cons staff-mem staff-list)]
        [else (cons (first staff-list) (add-staff  (rest staff-list) staff-mem))]))

(define (update-staff-info staff-list staff-mem)
  (cond [(empty? staff-list) (list staff-mem)]
        [(= (staff-member-id staff-mem) (staff-member-id (first staff-list))) (cons staff-mem (rest staff-list))]
        [(< (staff-member-id staff-mem) (staff-member-id (first staff-list))) (cons staff-mem staff-list)]
        [else (cons (first staff-list) (update-staff-info (rest staff-list) staff-mem))]))

(define (all-staff-info staff-list)
  (cond [(empty? staff-list) empty]
        [else (cons (string-append (number->string (staff-member-id (first staff-list))) " "                                   
                                   (staff-member-name (first staff-list)) " "
                                   (staff-member-dept (first staff-list))) (all-staff-info (rest staff-list)))]))

(define (func1 staff-list dept m)
   (cond [(empty? staff-list) 0]
         [(equal? dept (staff-member-dept (first staff-list)))
          (+ 1 (func1 (rest staff-list) dept m))]
         [else (+ 0 (func1 (rest staff-list) dept m))]))
         
(define (count-staff-by-dept staff-list dept-list)
  (cond [(empty? dept-list) empty]
        [else (cons (func1 staff-list (first dept-list) 0)
                    (count-staff-by-dept staff-list (rest dept-list)))]))

(define (no-staff staff-list dept)
  
  (cond
    [(empty? staff-list) 0]
    [(string=? (staff-member-dept (first staff-list)) dept)
     (+ 1 (no-staff (rest staff-list) dept))]
    [else (no-staff (rest staff-list) dept)]))

(define (staff-sal staff-mem sal-list)
  
  (cond
    [(empty? sal-list) 0]
    [(= (staff-member-id staff-mem) (salary-staff-id (first sal-list)))
        (+ (salary-base (first sal-list))
           (salary-bonus (first sal-list)))]
    [else (staff-sal staff-mem (rest sal-list))]))

(define (sal-dept staff-list sal-list dept)
  (cond
    [(empty? staff-list) 0]
    [(string=? (staff-member-dept (first staff-list)) dept)
     (+ (staff-sal (first staff-list) sal-list)
        (sal-dept (rest staff-list) sal-list dept))]
    [else (sal-dept (rest staff-list) sal-list dept)]))

(define (avg-salary-dept staff-list salary-list dept)
  (cond
    [(zero? (no-staff staff-list dept)) 0]
    [else (/ (sal-dept staff-list salary-list dept)
             (no-staff staff-list dept))]))

(define (avg-salary-by-dept staff-list salary-list dept-list)
  (cond
    [(empty? dept-list) empty]
    [else (cons (avg-salary-dept staff-list salary-list (first dept-list))
          (avg-salary-by-dept staff-list salary-list (rest dept-list)))]))
  
        






                                   
  





























  


