;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname applicants) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp") (lib "guess-gui.rkt" "teachpack" "htdp") (lib "gui.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 05, Problem 3
;;***************************************************
;;

(define-struct pprof (title wts))
;; A position profile (PProf) is a (make-pprof Sym (listof Nat))

;; PART A -------------------------------------------------


;;(applicant-score applicant-ranking-list position-profile) produces an
;;    applicant score by using applicant-ranking-list and position-profile.
;;
;;applicant-score: (listof Nat) PProf -> Nat
;; requires No of elements in applicant-ranking-list is equal to
;;    no of elements in PProf-wts

;;Examples:
(check-expect (applicant-score (list 0) (make-pprof 'SWD (list 4))) 0)
(check-expect (applicant-score  empty (make-pprof 'SWD empty)) 0)

(define (applicant-score applicant-ranking-list position-profile)
  
  (cond [(empty? applicant-ranking-list) 0]
        [else (+ (* (first applicant-ranking-list)
                    (first (pprof-wts position-profile)))
           (applicant-score (rest applicant-ranking-list)
           (make-pprof (pprof-title position-profile)
                       (rest (pprof-wts position-profile)))))]))

;;Tests:
(check-expect (applicant-score (list 1 2 3 4)
                               (make-pprof 'SWD (list 4 2 8 1))) 36)

(check-expect (applicant-score (list 0 2 3 0)
                               (make-pprof 'SWD (list 4 2 8 1))) 28)

(check-expect (applicant-score (list 0 0 0 0)
                               (make-pprof 'SWD (list 4 2 8 1))) 0)

(check-expect (applicant-score (list 1 2 3 1)
                               (make-pprof 'SWD (list 0 0 0 0))) 0)

(check-expect (applicant-score (list 1 1 1 1)
                               (make-pprof 'SWD (list 9 9 9 9))) 36)

(check-expect (applicant-score (list 1 2) (make-pprof 'SWD (list 1 2))) 5)

(check-expect (applicant-score (list 1 2 3) (make-pprof 'SWD (list 0 1 2))) 8)


;; PART B -------------------------------------------------


;;(position-max applicant-ranking-list position-profile-1 position-profile-2)
;;   produces which job is more suitable for the applicant b/w position-profile-1
;;   and position-profile-2 using his applicant-ranking-list.
;;
;;position-max: (listof Nat) PProf PProf -> PProf
;;
;;Examples:
(check-expect(position-max (list 0 0 0 0) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 6 3 1 4)))
                                          (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-max (list 8 1 1 1) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 6 3 1 4)))
                                          (make-pprof 'SWD (list 6 3 1 4)))

(define (position-max applicant-ranking-list position-profile-1 position-profile-2)
  (cond [(> (applicant-score applicant-ranking-list position-profile-1)
            (applicant-score applicant-ranking-list position-profile-2))
             position-profile-1]
        [(< (applicant-score applicant-ranking-list position-profile-1)
            (applicant-score applicant-ranking-list position-profile-2))
             position-profile-2]
        [else position-profile-1]))

;;Tests:
(check-expect(position-max (list 1 2 3 1) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 6 3 1 4)))
                                          (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-max (list 1 0 0 0) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 0 0 0 0)))
                                          (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-max (list 1 1 1 1) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 2 1 7 9)))
                                          (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-max (list 1 2 3 4) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 2 1 7 9)))
                                          (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-max (list 1 1 1 1) (make-pprof 'SWD (list 0 0 5 5))
                                          (make-pprof 'SWD (list 5 5 0 0)))
                                          (make-pprof 'SWD (list 0 0 5 5)))

(check-expect(position-max (list 0 0 0 1) (make-pprof 'SWD (list 2 1 7 9))
                                          (make-pprof 'SWD (list 1 1 1 1)))
                                          (make-pprof 'SWD (list 2 1 7 9)))


;; PART C -------------------------------------------------


;;(best-position applicant-ranking-list position-profiles-list) produces
;;     the best position for the applicant by using his applicant-rating-list
;;     and list of positions.
;;
;;best-position: (listof Nat) (listof PProf) -> PProf
;;
;;Examples:
(check-expect(best-position (list 1 2 3 1)
                            (list (make-pprof 'SWD (list 1 1 1 9))
                            (make-pprof 'Prof (list 2 1 8 9))
                            (make-pprof 'TA (list 5 5 5 5))))
                            (make-pprof 'Prof (list 2 1 8 9)))

(check-expect(best-position (list 1 0 0 0)
                            (list (make-pprof 'SWD (list 1 1 1 9))
                            (make-pprof 'Prof (list 2 1 8 9))
                            (make-pprof 'TA (list 5 5 5 5))))
                            (make-pprof 'TA (list 5 5 5 5)))

(define (best-position applicant-ranking-list position-profiles-list)
  
  (cond [(empty? position-profiles-list) (make-pprof 'xyz (list 0 0 0 0))]
        [else (position-max applicant-ranking-list
                            (first position-profiles-list)
              (best-position applicant-ranking-list
                             (rest position-profiles-list)))]))

;;Tests:
(check-expect(best-position (list 1 0 0 1)
                            (list (make-pprof 'SWD (list 1 1 1 9))
                            (make-pprof 'Prof (list 2 1 8 9))
                            (make-pprof 'TA   (list 5 5 5 5))))
                            (make-pprof 'Prof (list 2 1 8 9)))


;;(position-list-max applicant-ranking-list position-profiles-list) is a wrapper
;;    function and produces the best position for the applicant by using
;;     applicant-ranking-list and position-profiles-list.
;;
;;position-list: (listof Nat) (listof PProf) -> PProf
;;
;;Examples:
(check-expect(position-list-max (list 1 2 3 1)
                     (list (make-pprof 'SWD (list 1 1 1 9))
                           (make-pprof 'Prof (list 2 1 8 9))
                           (make-pprof 'TA (list 5 5 5 5))))
                           (make-pprof 'Prof (list 2 1 8 9)))

(define (position-list-max applicant-ranking-list position-profiles-list)
  
  (cond [(empty? (rest position-profiles-list)) (first position-profiles-list)]
        [else (best-position applicant-ranking-list position-profiles-list)] ))

;;Tests:
(check-expect(position-list-max (list 7 2 3 2)
                     (list (make-pprof 'SWD (list 2 1 7 9))
                           (make-pprof 'Manager (list 0 8 2 9))
                           (make-pprof 'Assistant (list 5 9 5 1))))
                           (make-pprof 'Assistant (list 5 9 5 1)))

(check-expect(position-list-max (list 7 2 3 2)
                     (list (make-pprof 'SWD (list 2 1 7 9))))
                           (make-pprof 'SWD (list 2 1 7 9)))

(check-expect(position-list-max (list 1 2 3 1)
                     (list (make-pprof 'SWD (list 1 1 1 9))
                           (make-pprof 'Prof (list 1 1 0 9))
                           (make-pprof 'TA (list 0 0 5 0))))
                           (make-pprof 'SWD (list 1 1 1 9)))


;; PART D -------------------------------------------------


;;(contain? position-title position-profiles-list) checks if the position-title
;;    is contained in the position-profiles-list.
;;
;;contain?: Sym (listof PProf) -> (listof Bool)
;;
;;Examples:
(check-expect (contain? 'A (list
                                (make-pprof 'A (list 1 2))
                                (make-pprof 'B (list 9 2))
                                (make-pprof 'X (list 1 2))
                                (make-pprof 'C (list 5 5))
                                (make-pprof 'D (list 4 4))))
                            (list false true true true true))

(check-expect (contain? 'B (list
                                (make-pprof 'B (list 9 2))
                                (make-pprof 'X (list 1 2))
                                (make-pprof 'C (list 5 5))
                                (make-pprof 'D (list 4 4))))
                                (list false true true true))

(define (contain? position-title position-profiles-list)
  
   (cond [(empty? position-profiles-list) empty]
         [(symbol=? position-title
                    (pprof-title (first position-profiles-list)))
          (cons false
                (contain? position-title
                          (rest position-profiles-list)))]
         [else (cons true
                     (contain? position-title
                               (rest position-profiles-list)))]))

;;Tests:
(check-expect (contain? 'B empty) empty)

(check-expect (contain? 'B (list
                                (make-pprof 'C (list 9 2))
                                (make-pprof 'X (list 1 2))))
                           (list true true))
                              

;;(remove-position position-title position-profiles-list) removes the
;;    position-profile with position-title as it's titel
;;    from position-profiles-list.
;;
;;remove-position: Sym (listof PProf) -> (listof PProf)
;;
;;Examples:
(check-expect (remove-position 'SWD (list (make-pprof 'SWD (list 1 1 1 9))
                                          (make-pprof 'PWD (list 1 1 0 9))
                                          (make-pprof 'XWD (list 0 0 5 0))))
                                     (list(make-pprof 'PWD (list 1 1 0 9))
                                          (make-pprof 'XWD (list 0 0 5 0))))

(check-expect (remove-position 'SWD (list (make-pprof 'EWD (list 1 1 1 9))
                                          (make-pprof 'PWD (list 1 1 0 9))
                                          (make-pprof 'XWD (list 0 0 5 0))))
                                     (list(make-pprof 'EWD (list 1 1 1 9))
                                          (make-pprof 'PWD (list 1 1 0 9))
                                          (make-pprof 'XWD (list 0 0 5 0))))

(define (remove-position position-title position-profiles-list)
  
  (cond [(empty? position-profiles-list) empty]
        [(equal? true (first(contain? position-title position-profiles-list)))
         (cons (first position-profiles-list)
               (remove-position position-title
               (rest position-profiles-list)))]
        
        [else (remove-position position-title
              (rest position-profiles-list))]))

;;Tests:
(check-expect (remove-position 'A empty) empty)
                               
(check-expect (remove-position 'SWD (list (make-pprof 'SWD (list 1 2)))) empty)

(check-expect (remove-position 'A    (list (make-pprof 'B (list 1))
                                           (make-pprof 'A (list 1)))) 
                                     (list (make-pprof 'B (list 1))))





                                      




         
            









