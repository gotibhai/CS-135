;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))

(define G '((A (C D E))
(B (E J))
(C ())
(D (F J))
(E (K))
(F (K H))
(H ())
(J (H))
(K ())))

;;(neighbours v G) takes in a node and a graph and return a list of
;;  its neighbours
;;neighbours : Node Graph -> (listof Node)

(define (neighbours v G)
  
  (cond [(empty? G) (error "vertex not in graph")]
        [(symbol=? v (first (first G))) (second (first G))]
        [else (neighbours v (rest G))]))

;;(find-route orig dest G) takes in a orig , a dest and a Graph G
;;   and returns a route from orig to dest if it exists.
;;
;;find-route: Node Node Graph -> (listof Node)

(define (find-route orig dest G)
  
  (cond [(symbol=? orig dest) (list orig)]
        [else (local [(define nbrs (neighbours orig G))
                (define route (find-route/list nbrs dest G))]
                (cond [(false? route) false]
                      [else (cons orig route)]))]))

;; (find-route/list los dest G) produces route from
;; an element of los to dest in G, if one exists.
;;
;; find-route/list: (listof Node) Node Graph -> (anyof (listof Node) false)

(define (find-route/list los dest G)
  
  (cond [(empty? los) false]
        [else (local [(define list-route (find-route (first los) dest G))]
                (cond [(false? list-route)
                       (find-route/list (rest los) dest G)]
                      [else list-route]))]))
                      
                
                      
                      
                      
       
                













         


                   
  