;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname extract-wpos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct wpos (row col horiz? len))

(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-abc2 '((#\A #\B #\C) (#\X #\Y #\Z) (#\# #\# #\#) (#\. #\# #\#)))

;;TRANSPOSEEEE START ------------------
(define (first-ele grid)
  
  (cond [(empty? grid) empty]
        [else(cons (first (first grid)) (first-ele (rest grid)))]))

(define (strip-1 grid)
  (map rest grid))

(define (transpose grid)
  
  (cond [(empty? grid) empty]
         [(empty? (first grid)) empty]
        [else (cons (first-ele grid) (transpose (strip-1 grid)))]))

;;TRANSPOSEEEE END------------------

(define (iterate grid x y dest-x dest-y len)
  (cond [(empty? grid) empty]
        [(= x dest-x) (cond [(= y dest-y) (prt grid len 0)]
                            [else (iterate (strip-1 grid) x (add1 y) dest-x dest-y len)])]
        [else (iterate (rest grid) (add1 x) y dest-x dest-y len)]))                  

(define (prt grid len z)
  (cond [(= z len) empty]
        [else (cons (first (first grid)) (prt (strip-1 grid) len (add1 z)))]))

(define (extract-wpos g wp)
  (cond [(boolean=? true (wpos-horiz? wp)) (iterate g 0 0 (wpos-row wp) (wpos-col wp) (wpos-len wp))]
        [else (iterate (transpose g) 0 0 (wpos-col wp) (wpos-row wp) (wpos-len wp))]))


;(check-expect (extract-wpos grid-abc2 (make-wpos 0 2 false 4)) (list #\C #\Z #\# #\#))
;(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
;(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
;(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(extract-wpos (list
   (list #\. #\. #\. #\. #\. #\. #\. #\. #\K #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\A #\D #\A #\M #\.)
   (list #\. #\. #\. #\. #\. #\. #\. #\. #\R #\. #\.)
   (list #\. #\. #\. #\. #\. #\A #\L #\B #\E #\R #\T)
   (list #\. #\. #\. #\. #\. #\. #\E #\. #\N #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\S #\. #\. #\. #\.)
   (list #\. #\. #\. #\. #\. #\. #\L #\. #\. #\. #\.)
   (list #\. #\. #\. #\# #\# #\# #\E #\. #\. #\. #\.)
   (list #\. #\. #\. #\# #\. #\. #\Y #\. #\. #\. #\.)
   (list #\# #\# #\# #\# #\. #\. #\. #\. #\. #\. #\.))(make-wpos 7 3 #true 4))