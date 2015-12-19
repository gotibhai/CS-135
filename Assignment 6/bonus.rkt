;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(define (add-doctor doc doctors)
  (cond
    [(empty? doctors) (cons doc doctors)]
    [(< (first doc)(first (first doctors))) (cons doc doctors)]
    [else (cons (first doctors) (add-doctor doc (rest doctors)))]))


(define (init-doctors n)
  (cond [(equal? n 0) empty]
        [else (cons (list 0 empty) (init-doctors (- n 1)))]))


(define (allocate-doctors doctors patients)
  (cond
    [(empty? patients) doctors]
    [else (allocate-doctors (add-doctor
                              (list (+ (first (first doctors)) (* 10 (ceiling (first (first patients)))))
                                    (cons (second (first patients)) (second (first doctors))))
                              (rest doctors))
                            (rest patients))]))


(define (doctor-allocation n patients)
  (allocate-doctors (init-doctors n) patients))

