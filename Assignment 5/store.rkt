;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname store) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 05, Problem 2
;;***************************************************
;;

;; PART A -------------------------------------------------


(define-struct product (name price taxable?))
;;A product is a (make-product Sym Num Bool)
;;               requires: price > 0
;; price cannot have fractional cents


;;(have-product? symbol product-list) checks if the symbol is the name
;;      of any of the products in the list.
;;
;;have-product?: Sym (listof Product) -> Bool
;;
;;Examples:
(check-expect (have-product? 'man (cons (make-product 'man 100 true)
                                  (cons (make-product 'woman 110 true)
                    (cons (make-product 'poman 120 true) empty)))) true)

(check-expect (have-product? 'doman (cons (make-product 'man 100 true)
                                    (cons (make-product 'woman 110 true)
                                                        empty))) false)

(define (have-product? symbol product-list)
  
  (cond [(empty? product-list) false]
        [(symbol=? symbol (product-name (first product-list))) true]
        [else (have-product? symbol (rest product-list))]))

;;Tests:

(check-expect (have-product? 'heman empty) false)

(check-expect (have-product? 'doman (list (make-product 'doman 100 true))) true)
                                                                     
(check-expect (have-product? 'man (list (make-product 'pman 100 true)
                                        (make-product 'woman 110 true)
                                        (make-product 'man 120 true))) true)

(check-expect (have-product? 'man (list (make-product 'doman 100 true)
                                        (make-product 'man 110 true)
                                        (make-product 'poman 120 true))) true)

(check-expect (have-product? 'man (list (make-product 'woman 100 true)
                                        (make-product 'doman 110 true)
                                        (make-product 'poman 120 true))) false)

(check-expect (have-product? 'man (list (make-product 'woman 100 true))) false)
                                                     
                                                     
;; PART B -------------------------------------------------


;;(product-name-list product-list) produces a list of product-names from
;;     a product-list.
;;
;;product-name-list: (listof Product) -> (listof Sym)
;;
;;Examples:
(check-expect (product-name-list (list (make-product 'A 10 true)
                                       (make-product 'B 10 true)
                                       (make-product 'C 10 true)))
                                 (list 'A 'B 'C))

(check-expect (product-name-list (list (make-product 'A 100 true)
                                       (make-product 'B 30 true)))
                                 (list 'A 'B))

(define (product-name-list product-list)
  
  (cond [(empty? product-list) empty]
        [else (cons (product-name (first product-list))
            (product-name-list (rest product-list)))]))

;;Tests:
(check-expect (product-name-list (list (make-product 'A 10 true)))(list 'A))

(check-expect (product-name-list empty)empty)


;;(valid-order? symbol-list product-list) tells us if all symbols in symbol-list
;; are names of items in the product-list.
;;
;;valid-order?: (listof Sym) (listof Product) -> Bool
;;
;;Examples:
(check-expect (valid-order? (list 'man 'woman)
                            (list (make-product 'man 100 true)
                            (make-product 'woman 110 true))) true)

(check-expect (valid-order? (list 'man 'woman)
                            (list (make-product 'man 100 true)
                                  (make-product 'Fooman 110 true))) false)

(define (valid-order? symbol-list product-list)
  
  (cond [(empty? symbol-list) true]
        [(empty? product-list) false]
        [(member? (first symbol-list) (product-name-list product-list))
         (valid-order? (rest symbol-list) product-list)]
        [else false]))

;;Tests:
(check-expect (valid-order? empty
                (list (make-product 'man 100 true)
                      (make-product 'woman 110 true))) true)

(check-expect (valid-order? empty empty) true)

(check-expect (valid-order? (list 'man) empty) false)

(check-expect (valid-order? (list 'man)
                (list (make-product 'man 100 false)
                      (make-product 'woman 110 true))) true)

(check-expect (valid-order? (list 'man 'boy 'D)
                (list (make-product 'man 10 false)
                      (make-product 'boy 10 true)
                      (make-product 'C 10 false))) false)

(check-expect (valid-order? (list 'man)
                (list (make-product 'yolo 20 false)
                      (make-product 'Bolo 30 true)
                      (make-product 'Colo 40 false))) false)

(check-expect (valid-order? (list 'man 'woman 'Dog)
                (list (make-product 'man 20 false)
                      (make-product 'woman 30 true)
                      (make-product 'Dog 40 false)
                      (make-product 'Xorax 40 false))) true)

(check-expect (valid-order?
               (list 'man 'man 'man)
               (list (make-product 'man 20 false)
                     (make-product 'Boy 30 true)
                     (make-product 'Cat  40 false)
                     (make-product 'Xorax 40 false))) true)

(check-expect (valid-order? (list 'man 'man 'Dog)
               (list (make-product 'man 20 false)
                     (make-product 'woman 30 true)
                     (make-product 'Cat 40 false)
                     (make-product 'Xorax 40 false))) false)


;; PART C ------------------------------------------------- 


;;(total-price product-list) takes in a product list and produces a new
;;    list with new prices with tax depending on whether they're taxable or not.
;;
;;total-price: (listof Product) -> (listof Product)
;;
;;Examples:
(check-expect (total-price (list (make-product 'man 20 false)
                                 (make-product 'man 30 true)
                                 (make-product 'Cat 40 false)
                                 (make-product 'Xorax 40 false)))
                           (list (make-product 'man 20 false)
                                 (make-product 'man 33.9 true)
                                 (make-product 'Cat 40 false)
                                 (make-product 'Xorax 40 false)))

(check-expect (total-price (list (make-product 'man 20 true)
                                 (make-product 'man 30 true)
                                 (make-product 'Cat 40 true)
                                 (make-product 'Xorax 40 true)))
                           (list (make-product 'man 22.6 true)
                                 (make-product 'man 33.9 true)
                                 (make-product 'Cat 45.2 true)
                                 (make-product 'Xorax 45.2 true)))

(define tax-rate 1.13)

(define (total-price product-list)
  
  (cond [(empty? product-list) empty]
        [(equal? true  (product-taxable? (first product-list)))
         (cons (make-product (product-name (first product-list))
                          (* tax-rate (product-price (first product-list)))
                          (product-taxable? (first product-list)))
                          (total-price (rest product-list)))]
        
        [else (cons (make-product (product-name (first product-list))
                          (product-price (first product-list))
                          (product-taxable? (first product-list)))
                          (total-price (rest product-list)))]))

;;Tests:

(check-expect (total-price empty) empty)

(check-expect (total-price (list (make-product 'man 100 false)
                                 (make-product 'man 30 false)
                                 (make-product 'Cat 40 false)))
                           (list (make-product 'man 100 false)
                                 (make-product 'man 30 false)
                                 (make-product 'Cat 40 false)))

(check-expect (total-price (list (make-product 'man 100 false)
                                 (make-product 'man 30 false)
                                 (make-product 'Cat 40 true)))
                           (list (make-product 'man 100 false)
                                 (make-product 'man 30 false)
                                 (make-product 'Cat 45.2 true)))


;;(budget-items product-list max-price) takes in a product list and a max-price
;;    and return a new product list of items whose price is less than max-price.
;;
;;budget-items: (listof Product) Num -> (listof Product)
;;              requires max-price >= 0
;;
;;Examples:
(check-expect (budget-items  (list (make-product 'man 0 true)
                                   (make-product 'woman 500 true)
                                   (make-product 'woman 150 true)) 100)
                                   (list (make-product 'man 0 true)))

(check-expect (budget-items  (list (make-product 'man 100 true)
                                   (make-product 'woman 500 true)
                                   (make-product 'woman 150 true)) 100)
                                   (list))

(define (budget-items product-list max-price)
  
  (cond [(empty? product-list) empty]
        [(>= max-price (product-price (first (total-price product-list))))
        (cons (first (total-price product-list))(budget-items (rest product-list) max-price))]
        [else (budget-items (rest product-list) max-price)]))

;;Tests
(check-expect (budget-items (list (make-product 'man 100 false)
                                  (make-product 'woman 50 true)
                                  (make-product 'woman 150 true)) 100)
                            (list (make-product 'man 100 false)
                                  (make-product 'woman 56.5 true)))

(check-expect (budget-items (list (make-product 'man 99 false)
                                  (make-product 'woman 100 true)) 100)
                            (list (make-product 'man 99 false)))

(check-expect (budget-items (list (make-product 'man (/ 100 1.13) true)
                                  (make-product 'woman 100 true)) 100)
                            (list (make-product 'man 100 true)))

(check-expect (budget-items (list (make-product 'man 100 false)
                                  (make-product 'woman 50 true)
                                  (make-product 'woman 15 true)) 1)empty)
                                                             
(check-expect (budget-items (list (make-product 'man 100 false)
                                  (make-product 'woman 50 true)
                                  (make-product 'woman 15 true)) 50)
                                  (list (make-product 'woman 16.95 true)))

(check-expect (budget-items (list (make-product 'man 100 false)
                                  (make-product 'woman 50 true)
                                  (make-product 'woman 15 true)) 100)
                            (list (make-product 'man 100 false)
                                  (make-product 'woman 56.5 true)
                                  (make-product 'woman 16.95 true)))

(check-expect (budget-items (list (make-product 'man 0 false)
                                  (make-product 'woman 0 true)
                                  (make-product 'woman 0 true)) 1)
                            (list (make-product 'man 0 false)
                                  (make-product 'woman 0 true)
                                  (make-product 'woman 0 true)))

(check-expect (budget-items (list (make-product 'man 0 false)
                                  (make-product 'woman 0 true)
                                  (make-product 'woman 0 true)) 0)
                            (list (make-product 'man 0 false)
                                  (make-product 'woman 0 true)
                                  (make-product 'woman 0 true)))

(check-expect (budget-items (list (make-product 'man 10 false)
                                  (make-product 'woman 10 true)
                                  (make-product 'woman 10 true)) 0)empty)

(check-expect (budget-items (list (make-product 'man 0 false)
                                  (make-product 'woman 10 true)
                                  (make-product 'woman 10 true)) 0)
                                  (list (make-product 'man 0 false)))


;; PART D -------------------------------------------------


;;(sym-in-product symbol product-list) takes in a symbol and a product
;;    list and returns the product price if the symbol matches with a
;;    product name.
;;
;;sym-in-product: Sym (listof Product) -> Num
;;
;;Examples:
(check-expect (sym-in-product 'man (list (make-product 'man 10 false)
                                         (make-product 'woman 10 true)
                                         (make-product 'woman 10 true))) 10)

(check-expect (sym-in-product 'boman (list (make-product 'man 10 false)
                                           (make-product 'woman 10 true)
                                           (make-product 'woman 10 true))) 0)

(define (sym-in-product symbol product-list)
  
  (cond [(empty? product-list) 0]
        [(symbol=? symbol (product-name (first product-list)))
         (product-price (first product-list))]
        [else (sym-in-product symbol (rest product-list))]))

;;Tests:
(check-expect (sym-in-product 'woman (list (make-product 'man 10 false)
                                           (make-product 'woman 10 true)
                                           (make-product 'woman 10 true))) 10)

(check-expect (sym-in-product 'woman (list (make-product 'man 10 false)
                                           (make-product 'woman 20 true)
                                           (make-product 'woman 10 true))) 20)


;;(add-price symbol-list product-list) takes in a symbol list
;;    and a product-list and returns the total price of symbols
;;    in product-list.
;;
;;add-price: (listof Sym) (listof Product) -> Num
;;
;;Examples:

(check-expect (add-price (list 'man 'Dog)
               (list (make-product 'man 20 false)
                     (make-product 'woman 30 true))) 20)

(check-expect (add-price empty
               (list (make-product 'man 20 false)
                     (make-product 'woman 30 true))) 0)

(define (add-price symbol-list product-list)
  
  (cond [(empty? symbol-list) 0]
        [else  (+ (sym-in-product (first symbol-list) product-list)
                    (add-price (rest symbol-list) product-list))]))

;;Tests:

(check-expect (add-price (list 'man 'woman)
               (list (make-product 'man 20 false)
                     (make-product 'woman 30 true))) 50)

(check-expect (add-price (list 'man 'woman 'man)
               (list (make-product 'man 20 false)
                     (make-product 'woman 30 true))) 70)

(check-expect (add-price (list 'man 'woman)
               (list (make-product 'man 20 false))) 20)

(check-expect (add-price (list 'man 'woman) empty) 0)


;;(total-order symbol-list product-list) takes in a symbol list
;;    and a product-list checks if its valid order if yes, returns
;;    the total price of symbols.
;;
;;total-order: (listof Sym) (listof Product) -> (anyof Sym Num)
;;
;;Examples:
(check-expect (total-order (list 'woman)
                           (list (make-product 'woman 100 false)
                                 (make-product 'man 10 true))) 100)

(check-expect (total-order (list 'man 'woman)
                           (list (make-product 'woman 100 false)
                                 (make-product 'man 50 true))) 156.5)

(define (total-order symbol-list product-list)
  
  (cond [(not (valid-order? symbol-list (total-price product-list)))
                                                    'cannot-fulfill]
        [else (/ (floor (* (add-price symbol-list
                           (total-price product-list)) 100)) 100)]))

;;Tests:
(check-expect (total-order (list 'dog 'woman)
                           (list (make-product 'woman 100 false)
                                 (make-product 'man 56.5 true))) 'cannot-fulfill)

(check-expect (total-order (list 'man 'woman)
                           (list(make-product 'woman 100 true)
                                (make-product 'man 56.5 true))) 176.84)

(check-expect (total-order empty
                           (list(make-product 'woman 100 true)
                                (make-product 'man 56.5 true))) 0)

(check-expect (total-order empty empty) 0)

(check-expect (total-order (list 'man 'woman) empty) 'cannot-fulfill)

(check-expect (total-order (list 'man 'woman) 
                           (list (make-product 'woman 100 false)
                                 (make-product 'man 50 true))) 156.5)

(check-expect (total-order (list 'man 'woman) 
                           (list (make-product 'woman 100 false)
                                 (make-product 'man 50 false))) 150)

(check-expect (total-order (list 'man 'woman) 
                           (list (make-product 'woman 0 false)
                                 (make-product 'man 0 false))) 0)

(check-expect (total-order (list 'man 'woman) 
                           (list (make-product 'woman 0 true)
                                 (make-product 'man 0 true))) 0)

(check-expect (total-order (list 'man) 
                           (list (make-product 'woman 0 true)
                                 (make-product 'lol 0 true))) 'cannot-fulfill)

(check-expect (total-order (list 'man) 
                           (list (make-product 'man 0 true))) 0)

(check-expect (total-order (list 'man 'man) 
                           (list (make-product 'man 10 true))) 22.6)

(check-expect (total-order (list 'man 'man) 
                           (list (make-product 'woman 10 true))) 'cannot-fulfill)







































