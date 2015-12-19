;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A BST is one of
;; * empty
;; * (make-node Int Any BST BST)

;; A binary tree (BT) is one of:
;; * empty
;; * Node

(define-struct node (key value left right))

(define (count-value bt v)
  (cond [(empty? bt) 0]
        [else (+ (cond [(string=? v (node-value bt)) 1]
                 [else 0])
                 (count-value (node-left bt) v)
                 (count-value (node-right bt) v))]))

;(count-value (make-node 4 "e" (make-node 2 "g" empty empty) (make-node 5 "g" empty empty)) "g")

(define (increment-tree bt x)
  (cond [(empty? bt) empty]
        [else (make-node (add1(node-key bt))
                         (node-value bt)
                         (increment-tree (node-left bt) x)
                         (increment-tree (node-right bt) x))]))

;(increment-tree (make-node 4 "e" (make-node 2 "g" empty empty) (make-node 5 "g" empty empty)) 1)

(define (search-bst bt x)
  
  (cond [(empty? bt) (error "Not found")]
        [(= (node-key bt) x) (node-value bt)]
        [(> x (node-key bt)) (search-bst (node-right bt) x)]
        [else (search-bst (node-left bt) x)]))

;(search-bst (make-node 4 "e" (make-node 2 "g" empty empty) (make-node 5 "g" empty empty)) 5)

(define (insert-bt key val bt)
  (cond [(empty? bt) (make-node key val empty empty)]
        [(> key (node-key bt)) (make-node (node-key bt) (node-value bt)
                              (node-left bt) (insert-bt key val (node-right bt)))]
        [(< key (node-key bt)) (make-node (node-key bt) (node-value bt)
                              (insert-bt key val(node-left bt)) (node-right bt))]
        [else (make-node key val (node-left bt) (node-right bt))]))

;; bst-min: BST → (anyof false (list Int Any))
(define (bst-min bst)
(cond [(empty? bst) false]
[(empty? (node-left bst)) (list (node-key bst) (node-value bst))]
[else (bst-min (node-left bst))]))


(define (remove-bt key bt)
  (cond [(empty? bt) empty]
        [(> key (node-key bt)) (make-node (node-key bt) (node-value bt)
                              (node-left bt) (remove-bt key (node-right bt)))]
        [(< key (node-key bt)) (make-node (node-key bt) (node-value bt)
                              (remove-bt key (node-left bt)) (node-right bt))]
        [(empty? (node-right bt)) (node-left bt)]
        [else (local [(define min-right (bst-min (node-right bt)))]
                (make-node (first min-right) (second min-right)
                           (node-left bt)
                           (remove-bt (first min-right)(node-right bt))))]))



(define-struct nodeva (key left right))

(define (insertbt key bt)
  (cond [(empty? bt) (make-nodeva key empty empty)]
        [(> key (nodeva-key bt)) (make-nodeva (nodeva-key bt) (nodeva-left bt)
                                (insertbt key (nodeva-right bt)))]
        [(< key (nodeva-key bt)) (make-nodeva (nodeva-key bt) 
                              (insertbt key (nodeva-left bt)) (nodeva-right bt))]
        [else (make-nodeva key (nodeva-left bt) (nodeva-right bt))]))

(define (lstobst ls)
  (cond [(empty? ls) empty]
        [else (insertbt (first ls) (lstobst (rest ls)))]))


(define (bsttolst bt)
  (cond [(empty? bt) empty]
        [else (append (bsttolst (node-left bt))
                      (cons (list (node-key bt) (node-value bt))
                            (bsttolst (node-right bt))))]))

(define (isort ls)
  (cond [(empty? ls) empty]
        [else (insert (isort (rest ls)) (first ls))]))

(define (insert ls x)
  (cond [(empty? ls) (list x)]
        [(> x (first ls)) (cons (first ls) (insert (rest ls) x))]
        [else (cons x ls)]))

(define (quick-sort ls fn)
  (cond [(empty? ls) empty]
        [else (local [ (define pivot (first ls))
                       (define less (filter (lambda (x) (fn x pivot)) (rest ls)))
                       (define more (filter (lambda (x) (not(fn x pivot))) (rest ls)))]
                (append (quick-sort less fn)
                        (list pivot)
                        (quick-sort more fn)))]))

(define (merge-sort ls fn)
  (local [(define y 0)

(define (merge ln1 ln2 fn)
(cond   [(and (empty? ln1) (cons? ln2)) ln2]
        [(and (empty? ln2) (cons? ln1)) ln1]
        [(and (cons? ln1) (cons? ln2))
  (cond [(fn (first ln1) (first ln2)) (cons (first ln1) (merge (rest ln1) ln2 fn))]
        [else (cons (first ln2) (merge ln1 (rest ln2) fn))])]))

(define (div ln mid y fn)
  (cond [(equal? fn <) (cond [(< y mid) (cons (list-ref ln y)
                                              (div ln mid (add1 y) <))]
                             [else empty])]
        [(equal? fn >) (cond [(= y (length ln)) empty]
                             [(>= y mid) (cons (list-ref ln y)
                                              (div ln mid (add1 y) >))])]))] 
       (cond [(empty? ls) empty]
        [(= 1 (length ls)) ls]
        [else (local [(define mid (cond [(= 0 (remainder (length ls) 2)) (/ (length ls) 2)]
                               [else (/ (add1 (length ls)) 2)]))
                      (define less (div ls mid y <))
                      (define more (div ls mid mid >))]
                (cond [(equal? fn <) (merge (merge-sort less fn) (merge-sort more fn) fn)]
                      [else (merge (merge-sort more fn) (merge-sort less fn) fn)]))])))
                      
  



                       
                        
                    



                 
                 
           


                         












              


                 
                 



