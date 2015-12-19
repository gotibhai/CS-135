;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname replace-wpos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

(define-struct wpos (row col horiz? len))

(define (strip-1 grid)
  (map rest grid))

(define (swap row len z loc)
  (cond [(= z len) (append loc row)]
        [else (swap (rest row) len (add1 z) loc)]))

(define (first-ele grid)
  
  (cond [(empty? grid) empty]
        [else(cons (first (first grid)) (first-ele (rest grid)))]))

(define (transpose grid)
  
  (cond [(empty? grid) empty]
         [(empty? (first grid)) empty]
        [else (cons (first-ele grid) (transpose (strip-1 grid)))]))

(define (iterate-x grid x y dest-x dest-y len loc)(cond
  [(empty? grid) empty]
  [(= x dest-x) (cons (iterate-y (first grid) y dest-y len loc) (rest grid))]
  [else (cons (first grid) (iterate-x (rest grid) (add1 x) y dest-x dest-y len loc))]))
(define (iterate-y row y dest-y len loc)(cond
  [(empty? row) empty]
  [(= y dest-y) (swap row len 0 loc)]
  [else (cons (first row) (iterate-y (rest row) (add1 y) dest-y len loc))]))

(define (replace-wpos g wp loc)
  
  (cond [(boolean=? true (wpos-horiz? wp)) (iterate-x g 0 0
                          (wpos-row wp) (wpos-col wp) (wpos-len wp) loc)]
        [else (iterate-x (transpose g) 0 0 (wpos-col wp)
                       (wpos-row wp) (wpos-len wp) loc)]))

(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
