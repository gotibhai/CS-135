;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees_practice) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
(define-struct binode (op arg1 arg2))
;; A binode is a (make-binode (anyof ’* ’+ ’/ ’-) BinExp BinExp)

;;A binary Arithmetic expression (BinExp) is one of:
;; * A Num
;; * A BINode

;;Examples:
;(make-binode '* 2 6)
;(make-binode '+ (make-binode '* 3 4) (make-binode '* 3 (make-binode '- 10 2)))

;;EVALUATING EXPRESSIONS
(define (my-binode bin1)
  (cond [(number? bin1) bin1]
        [(symbol=? (binode-op bin1) '*) (* (my-binode (binode-arg1 bin1))(my-binode (binode-arg2 bin1)))]
        [(symbol=? (binode-op bin1) '+) (+ (my-binode (binode-arg1 bin1))(my-binode (binode-arg2 bin1)))]
        [(symbol=? (binode-op bin1) '/) (/ (my-binode (binode-arg1 bin1))(my-binode (binode-arg2 bin1)))]
        [(symbol=? (binode-op bin1) '-) (- (my-binode (binode-arg1 bin1))(my-binode (binode-arg2 bin1)))]))

;;EXAMPLES
;(my-binode (make-binode '+ (make-binode '* 3 4) (make-binode '* 3 (make-binode '- 10 2))))


(define-struct node (key val left right))
;; A Node is a (make-node Num Str BT BT)

;;A binary tree (BT) is one of:
;;* empty
;;* Node

(define (count tree)
  (cond [(empty? tree) 0]
        [(number? tree) 1]
        [else ( + 1 (count (node-left tree))
                    (count (node-right tree)))]))
                    
         
;(count (make-node 2 3 (make-node 3 4 1 10) 30))

(define (counting-sim tree v)
  (cond [(empty? tree) 0]
        [(string=? v (node-val tree)) ( + 1 (counting-sim (node-left tree) v)
                                     (counting-sim (node-right tree) v))]
        [else (+ 0 (counting-sim (node-left tree)v)
                   (counting-sim (node-right tree) v))]))

;(counting-sim (make-node 2 "John" (make-node 3 "Pohn" empty empty) empty) "John")


;;CHECKS IF A KEY IS IN THE BT
(define (contain-bt? bt num)
  (cond [(empty? bt) false]
        [(=(node-key bt) num) true]
        [(contain-bt? (node-left bt) num) true]
        [else (contain-bt? (node-right bt) num)]))


;(contain-bt? (make-node 3 "John" empty (make-node 2 "Pohn" empty empty)) 5)


;;CHECKS IF A KEY IS IN THE BT, IF IT IS RETURNS IT'S VALUE

(define (contain-bt bt num)
  (cond [(empty? bt) false]
        [(= (node-key bt) num) (node-val bt)]
        [(contain-bt (node-left bt) num) true]
        [else (contain-bt (node-right bt) num)]))

(contain-bt (make-node 3 "John" empty (make-node 2 "Pohn" empty empty)) 3)


         
