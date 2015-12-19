;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname org-chart) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
(define-struct supervisor (id subordinates))
;; A Supervisor is a (make-supervisor Nat (listof Org-Chart)) ;; requires: id values are unique
;;
;; An Org-Chart is one of:
;;* Nat
;;* Supervisor

;(define (direct-reports orgchart id)
; (cond [(= id (supervisor-id orgchart)) empty]


(define id-found -1)

(define (recurse-lst org-chart-lst id)
  (cond
    [(empty? org-chart-lst) empty]
    [else (append (direct-reports (first org-chart-lst) id) (recurse-lst (rest org-chart-lst) id))]))

(define (direct-reports org-chart id)
  (cond
    [(and (number? org-chart)(= org-chart id)) empty]
    [(and (supervisor? org-chart) ( =(supervisor-id org-chart) id))
     (recurse-lst (supervisor-subordinates org-chart) id-found)]
    [(and (number? org-chart) (= id-found id)) (cons org-chart empty)]
    [(and (supervisor? org-chart) (= id-found id)) (cons (supervisor-id org-chart)
                                                         (recurse-lst (supervisor-subordinates org-chart) id))]
    [(supervisor? org-chart)
     (recurse-lst (supervisor-subordinates org-chart) id)]
    [else empty]))





(define (flat-list org-chart)
  
  (cond
    [(empty? org-chart) empty]
    [(supervisor? org-chart)
     (append (list (supervisor-id org-chart))
             (list- (supervisor-subordinates org-chart)))]
    [else empty]))

(define (list- list-sub)
  
  (cond
    [(empty? list-sub) empty]
    [(number? (first list-sub))
     (append (list (first list-sub))
             (list- (rest list-sub)))]
    [else (append (flat-list (first list-sub))
                  (list- (rest list-sub)))]))










