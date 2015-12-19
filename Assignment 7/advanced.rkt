;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname advanced) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 07, Problem 2
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

(define-struct staff-node (staff left right))
;; A Staff-Node is a (make-staff-node Staff-Member Staff-BST Staff-BST)
;; requires: ids of all Staff-Member on the left subtree
;;           are smaller than the id of Staff-Member
;;           ids of all Staff-Member on the right subtree
;;           are larger than the id of Staff-Member

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

(define staff-node1 (make-staff-node staff4
                         (make-staff-node staff2
                                    (make-staff-node staff1 empty empty)
                                    empty)
                         (make-staff-node staff7
                                    (make-staff-node staff5 empty empty)
                                    (make-staff-node staff8 empty empty))))

;; A Staff-BST is one of:
;;* empty
;;* Staff-Node


;;PART A---------------------------------------------------------------


;;(add-staff-bst bst staff-mem) adds a staff-mem to the pre-existing bst.
;;
;; add-staff-bst: Staff-BST Staff-Member -> Staff-BST
;; requires:
;; no staff-member with the same id is in the Staff-BST
;; Examples:

(check-expect (add-staff-bst empty staff1)
              (make-staff-node staff1 empty empty))
(check-expect
 (add-staff-bst
  (make-staff-node staff6
                   (make-staff-node staff4 empty empty)
                   (make-staff-node staff8 empty empty))
  staff3)
 (make-staff-node staff6
                  (make-staff-node staff4
                                   (make-staff-node staff3 empty empty)
                                   empty)
                  (make-staff-node staff8 empty empty)))


(define (add-staff-bst bst staff-mem)
  
  (cond [(empty? bst) (make-staff-node staff-mem empty empty)]
        [(< (staff-member-id (staff-node-staff bst))
                        (staff-member-id staff-mem))
         (make-staff-node (staff-node-staff bst) (staff-node-left bst)
                          (add-staff-bst (staff-node-right bst) staff-mem))]
        [(> (staff-member-id (staff-node-staff bst))
            (staff-member-id staff-mem))
         (make-staff-node (staff-node-staff bst)
                          (add-staff-bst (staff-node-left bst) staff-mem)
                                               (staff-node-right bst))]))


;;Tests:


(check-expect
 (add-staff-bst
  (make-staff-node staff5
                   (make-staff-node staff4 empty empty)
                   (make-staff-node staff6 empty
                                    (make-staff-node staff7 empty empty)))
  staff8)
 (make-staff-node staff5
                  (make-staff-node staff4 empty empty)
                  (make-staff-node staff6 empty
                                   (make-staff-node staff7
                                                    empty
                                                    (make-staff-node staff8
                                                                     empty empty)))))


(check-expect (add-staff-bst staff-node1 staff6)
              (make-staff-node staff4
              (make-staff-node staff2
              (make-staff-node staff1 empty empty) empty)
              (make-staff-node staff7 (make-staff-node staff5
               empty
              (make-staff-node staff6 empty empty))
              (make-staff-node staff8 empty empty))))

(check-expect (add-staff-bst
               (make-staff-node staff6
                                (make-staff-node staff4
                                                 (make-staff-node staff1
                                                                  empty empty)
                                                 empty)
                                (make-staff-node staff7 empty empty))
               staff8)
              (make-staff-node staff6
                               (make-staff-node staff4
                                                (make-staff-node staff1
                                                                 empty empty)
                                                empty)
                               (make-staff-node staff7 empty
                                                (make-staff-node staff8
                                                                 empty empty))))
(check-expect
 (add-staff-bst
  (make-staff-node staff8 (make-staff-node staff7 empty empty) empty)
  staff1)
 (make-staff-node staff8
                  (make-staff-node staff7
                                   (make-staff-node staff1
                                                    empty empty) empty)
                  empty))
(check-expect
 (add-staff-bst
  (make-staff-node staff1 empty (make-staff-node staff7 empty empty))
  staff8)
 (make-staff-node staff1 empty
                  (make-staff-node staff7
                                   empty
                                   (make-staff-node staff8
                                                    empty empty))))


;;PART B---------------------------------------------------------------


;;(create-staff-bst-from-list lostaff) creates a Staff Node from a list
;;   of staff members.
;;
;; create-staff-bst-from-list: (listof Staff-Member) -> Staff-BST
;; requires:
;; every staff-member has a unique id
;;
;; Examples:

(check-expect
 (create-staff-bst-from-list
  (list staff5 staff1 staff2 staff3))
 (make-staff-node staff3
                  (make-staff-node staff2
                                   (make-staff-node staff1
                                                    empty empty) empty)
                  (make-staff-node staff5 empty empty)))
(check-expect (create-staff-bst-from-list
               empty) empty)
(check-expect (create-staff-bst-from-list (list staff1 staff8 staff7))
              (make-staff-node staff7 (make-staff-node staff1 empty empty)
                               (make-staff-node staff8 empty empty)))

(define (create-staff-bst-from-list lostaff)
  
  (cond [(empty? lostaff) empty]
        [else (add-staff-bst (create-staff-bst-from-list (rest lostaff))
                                                     (first lostaff))]))

;;Tests:

(check-expect (create-staff-bst-from-list
              (list staff3 staff7 staff8 staff1 staff2))
(make-staff-node (make-staff-member 2 "Adam" "R&D")
(make-staff-node (make-staff-member 1 "John" "Engineering") empty empty)
(make-staff-node
(make-staff-member 8 "Josh" "Engineering")
(make-staff-node
(make-staff-member 7 "Leslie" "R&D")
(make-staff-node (make-staff-member 3 "Albert" "Engineering") empty empty)empty)
empty)))

(check-expect (create-staff-bst-from-list
               (list staff3 staff7 staff6))
              (make-staff-node staff6 (make-staff-node staff3 empty empty)
                               (make-staff-node staff7
                                                empty empty)))

(check-expect (create-staff-bst-from-list
               (list staff8))
              (make-staff-node staff8 empty empty))

(check-expect
 (create-staff-bst-from-list
  (list staff1 staff2 staff3 staff4))
 (make-staff-node staff4
                  (make-staff-node staff3
                                   (make-staff-node staff2
                                                    (make-staff-node staff1
                                                                   empty empty)
                                                    empty)
                                   empty)
                  empty))

;;PART C---------------------------------------------------------------


;;(staff-mem-sal staff-mem sal-list) finds salary of
;; staff-mem in sal-list.
;;
;;staff-mem-sal: Staff-Member Salary-List -> Num
;;
;;Examples:
(check-expect (staff-mem-sal staff4 (list salary1 salary2 salary3))
              0)
(check-expect (staff-mem-sal staff1 empty)
              0)

(define (staff-mem-sal staff-mem sal-list)
  (cond
    [(empty? sal-list) 0]
    [(= (staff-member-id staff-mem) (salary-staff-id (first sal-list)))
     (+ (salary-base (first sal-list))
        (salary-bonus (first sal-list)))]
    [else (staff-mem-sal staff-mem (rest sal-list))]))


;;Tests:
(check-expect (staff-mem-sal staff2 sal-list)
              74250)
(check-expect (staff-mem-sal staff2 (list salary2))
              74250)


;;(tree-to-list tree) converts a binary tree to a sorted list of
;; elements by id.
;;
;;tree-to-list: Staff-BST -> (listof Staff-Member)
;;
;;Examples:

(check-expect (tree-to-list empty) empty)

(check-expect (tree-to-list (make-staff-node staff6
                                         (make-staff-node staff5 empty empty)
                                         (make-staff-node staff7 empty empty)))
              (list staff5 staff6 staff7))

(define (tree-to-list tree)
  
  (cond
    [(empty? tree) empty]
    [else (append (tree-to-list (staff-node-left tree))
                  (cons (staff-node-staff tree)
                        (tree-to-list (staff-node-right tree))))]))

;;Tests:
(check-expect (tree-to-list (make-staff-node staff6
                                           empty
                                           empty))
              (list staff6))
(check-expect (tree-to-list (make-staff-node staff4
                                           (make-staff-node staff3 empty empty)
                                           empty))
              (list staff3 staff4))


;;(fire salary-list staff-mem-list threshold) produces list of people
;; to fire from staff-mem-list, by finding them in salary-list and then checks
;; is the salary is above threshold.
;;
;;fire: Salary-List Staff-List Num -> (listof Staff-Member)
;; requires: threshold>0
;;
;;Examples:
;;
;;Tested in wrapper function.

(define (fire salary-list staff-mem-list threshold)
  
  (cond
    [(empty? staff-mem-list) empty]
    [(> (staff-mem-sal (first staff-mem-list) salary-list) threshold)
     (cons (first staff-mem-list)
           (fire salary-list (rest staff-mem-list) threshold))]
    [else (fire salary-list (rest staff-mem-list) threshold)]))


;;Tests:
;;
;;Tested in wrapper function.


;;(who-to-fire salary-list staff-bst threshold) produces a Staff-List
;; to be fired from staff-bst and then mapping back to salary-list while
;; checking if the person has salary above threshold.
;;
;; who-to-fire: (listof Salary) Staff-BST Num -> (listof Staff-Member)
;; requires: threshold must be greater than 0
;;
;;Examples:

(check-expect (who-to-fire empty empty 234) empty)

(check-expect(who-to-fire empty (make-staff-node staff7 empty empty) 100)
             empty)


(define (who-to-fire salary-list staff-bst threshold)
  
  (fire salary-list (tree-to-list staff-bst) threshold))


;;Tests:

(check-expect
 (who-to-fire (list salary7 salary2 salary3 salary4 salary5 salary6)
              (make-staff-node staff3
                               (make-staff-node staff2
                                                (make-staff-node
                                                 staff1 empty empty)
                                                empty)
                               (make-staff-node staff4
                                                (make-staff-node staff7 empty
                                                                 empty)
                                                empty))
              74250)
 (list staff3 staff7))

(check-expect
 (who-to-fire (list salary7 salary2 salary3 salary1 salary4 salary5 salary6)
              (make-staff-node staff3
                               (make-staff-node staff2
                                                (make-staff-node
                                                 staff1 empty empty)
                                                empty)
                               (make-staff-node staff4
                                                empty
                                                empty))
              0.0001)
 (list staff1 staff2 staff3 staff4))

(check-expect
 (who-to-fire (list salary7 salary2 salary8 salary1 salary4 salary5 salary6)
              (make-staff-node staff3
                               (make-staff-node staff2
                                                (make-staff-node
                                                 staff1 empty empty)
                                                empty)
                               (make-staff-node staff4
                                                empty
                                                empty))
              1000000000000000)
 empty)

;;PART D---------------------------------------------------------------


;;(bst-min tree) takes in a tree and returns min element from the tree.
;;
;;bst-min: Staff-BST -> Staff-Member
;;
;;Examples:
(check-expect (bst-min staff-node1)
(make-staff-member 1 "John" "Engineering"))

(check-expect (bst-min (staff-node-right staff-node1))
(make-staff-member 5 "Anne" "Defence"))

(define (bst-min tree)
  
  (cond [(empty? tree) empty]
        [else
         (cond
           [(empty? (staff-node-left tree)) (staff-node-staff tree)]
           [else (bst-min (staff-node-left tree))])]))

;;Tests:

(check-expect (bst-min (staff-node-left staff-node1))
(make-staff-member 1 "John" "Engineering"))

(check-expect (bst-min (staff-node-right(staff-node-right staff-node1)))
(make-staff-member 8 "Josh" "Engineering"))

(check-expect (bst-min empty) empty)


;; (mem id bst) takes in an id and a bst and returns the node in bst
;;    with that id!
;;
;; mem: Nat Staff-BST -> Staff-Member
;; requires:
;; every staff-member has a unique id and id >0
;; 
;; Examples:

(check-expect (mem 3 staff-node1) empty)

(check-expect (mem 4 staff-node1) (make-staff-member 4 "Liz" "Finance"))

(define (mem id bst)
  
  (cond [(empty? bst) empty]
        [(= id (staff-member-id (staff-node-staff bst))) (staff-node-staff bst)]
        [(> id (staff-member-id (staff-node-staff bst))) (mem id (staff-node-right bst))]
        [(< id (staff-member-id (staff-node-staff bst))) (mem id (staff-node-left bst))]))

;;Tests:

(check-expect (mem 7 staff-node1) (make-staff-member 7 "Leslie" "R&D"))

(check-expect (mem 8 staff-node1) (make-staff-member 8 "Josh" "Engineering"))

;;(remove-from-bst staff-bst staff-id) takes in a staff-bst and a staff-id
;;   and removes the node with staff-id from the staff-bst.
;;
;; remove-from-bst: Staff-BST Nat -> Staff-BST
;;
;;Examples:

(check-expect
 (remove-from-bst
  (make-staff-node staff7 empty (make-staff-node staff8 empty empty))
  1)
 (make-staff-node staff7 empty (make-staff-node staff8 empty empty)))

(check-expect
 (remove-from-bst
  (make-staff-node staff3
                   (make-staff-node staff2
                                    (make-staff-node staff1
                                                     empty empty)
                                    empty)
                   empty)
  2) (make-staff-node staff3 (make-staff-node staff1 empty empty) empty))

(define (remove-from-bst staff-bst staff-id)
  
   (cond [(empty? staff-bst) empty]
         [(equal? (mem staff-id staff-bst) (staff-node-staff staff-bst))
          (cond [(and (equal? empty (staff-node-right staff-bst))
                      (equal? empty (staff-node-left staff-bst))) empty]

                
                [(equal? empty (staff-node-right staff-bst))
                (make-staff-node (staff-node-staff (staff-node-left staff-bst))
                                 (remove-from-bst (staff-node-left staff-bst)
                                                  (staff-member-id (staff-node-staff (staff-node-left staff-bst)))) empty)]

                
                [(equal? empty (staff-node-left staff-bst))
                (make-staff-node (staff-node-staff (staff-node-right staff-bst))  empty                             
                                 (remove-from-bst (staff-node-right staff-bst)
                                                  (staff-member-id (staff-node-staff(staff-node-right staff-bst)))))]
                
                [else (make-staff-node (bst-min (staff-node-right staff-bst))
                                 (staff-node-left staff-bst)
                                 (remove-from-bst (staff-node-right staff-bst)
                                                  (staff-member-id (bst-min (staff-node-right staff-bst)))))])]
         
         [(> staff-id (staff-member-id (staff-node-staff staff-bst)))
          (make-staff-node (staff-node-staff staff-bst)
                           (staff-node-left staff-bst)
                           (remove-from-bst (staff-node-right staff-bst) staff-id))]
         
         [else (make-staff-node (staff-node-staff staff-bst)
                                 (remove-from-bst (staff-node-left staff-bst) staff-id)
                                 (staff-node-right staff-bst))])) 


;; Tests: 
 (check-expect (remove-from-bst staff-node1 4)
               (make-staff-node
 (make-staff-member 5 "Anne" "Defence")
 (make-staff-node (make-staff-member 2 "Adam" "R&D") (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty) empty)
 (make-staff-node (make-staff-member 7 "Leslie" "R&D") empty (make-staff-node (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect (remove-from-bst staff-node1 8)
(make-staff-node
 (make-staff-member 4 "Liz" "Finance")
 (make-staff-node (make-staff-member 2 "Adam" "R&D") (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty) empty)
 (make-staff-node (make-staff-member 7 "Leslie" "R&D") (make-staff-node (make-staff-member 5 "Anne" "Defence") empty empty) empty)))

(check-expect (remove-from-bst staff-node1 2)
(make-staff-node
 (make-staff-member 4 "Liz" "Finance")
 (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty)
 (make-staff-node
  (make-staff-member 7 "Leslie" "R&D")
  (make-staff-node (make-staff-member 5 "Anne" "Defence") empty empty)
  (make-staff-node (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect (remove-from-bst staff-node1 1)
  (make-staff-node
  (make-staff-member 4 "Liz" "Finance")
  (make-staff-node (make-staff-member 2 "Adam" "R&D") empty empty)
  (make-staff-node
  (make-staff-member 7 "Leslie" "R&D")
  (make-staff-node (make-staff-member 5 "Anne" "Defence") empty empty)
  (make-staff-node (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect
 (remove-from-bst (make-staff-node staff4
                         (make-staff-node staff2
                                    empty
                                    (make-staff-node staff3 empty empty))
                         (make-staff-node staff7
                                    (make-staff-node staff5 empty empty)
                                    (make-staff-node staff8 empty empty))) 4)
(make-staff-node
 (make-staff-member 5 "Anne" "Defence")
 (make-staff-node (make-staff-member 2 "Adam" "R&D") empty (make-staff-node (make-staff-member 3 "Albert" "Engineering") empty empty))
 (make-staff-node (make-staff-member 7 "Leslie" "R&D") empty (make-staff-node (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect
 (remove-from-bst (make-staff-node staff4
                         (make-staff-node staff2
                                    empty
                                    (make-staff-node staff3 empty empty))
                         (make-staff-node staff7
                                    (make-staff-node staff5 empty empty)
                                    (make-staff-node staff8 empty empty))) 2)
(make-staff-node
 (make-staff-member 4 "Liz" "Finance")
 (make-staff-node (make-staff-member 3 "Albert" "Engineering") empty empty)
 (make-staff-node
  (make-staff-member 7 "Leslie" "R&D")
  (make-staff-node (make-staff-member 5 "Anne" "Defence") empty empty)
  (make-staff-node (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect
 (remove-from-bst
  (make-staff-node staff6
                   (make-staff-node staff4
                                    (make-staff-node staff1
                                                     empty empty)
                                    empty)
                   (make-staff-node staff7 empty empty))
  6)
 (make-staff-node staff7
                  (make-staff-node staff4 (make-staff-node staff1 empty empty)
                                   empty)
                  empty))
(check-expect
 (remove-from-bst
  (make-staff-node staff8
                   (make-staff-node staff5 empty
                                    (make-staff-node staff6 empty empty))              
                   empty)
  5)
 (make-staff-node staff8 (make-staff-node staff6 empty empty)
                  empty))

(check-expect (remove-from-bst
               (make-staff-node staff2
                                empty (make-staff-node staff5 empty empty))
               5)
              (make-staff-node staff2 empty empty))
(check-expect (remove-from-bst
               (make-staff-node staff1 empty empty)
               1) empty)



              
                
  
          
    
