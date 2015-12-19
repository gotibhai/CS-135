;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define (add-node node out in G)
  (cond[(empty? G)(list (list node out))]
       [(empty? in)(cons (list node out) G)]
       [(symbol=? (first in) (first (first G)))
        (add-node node out (rest in)
        (cons(list (first (first G)) (cons node (second (first G)))) (rest G)))]
       [else (cons (first G)
                   (add-node node out in (rest G)))]))

;(add-node 'S '(E) '(A C)  
;'((A (B C D))
;(B (E))
;(C ())
;(D (B C))
;(E ())))

(define (remove-vertex G node)
  (filter (lambda (x) (not (symbol=? node  (first x)))) G))

;; delete-node: Graph Node → Graph

(define (delete-node g node)
(map
(lambda (x) (list (first x)
(filter (lambda (y)
(not (symbol=? y node)))
(second x)))) (remove-vertex g node)))


                     
(delete-node '((A (B C D))
(B (E))
(C (A))
(D (B C A))
(E ())) 'A)





                  
                