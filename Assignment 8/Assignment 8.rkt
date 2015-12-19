;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct email-record (day-id hours-worked emails-sent))
;; An Email-Record is a (make-email-record Nat Num Nat)
;; requires: hours-worked >= 0

(define-struct daily-stats (staff-id staff-name emails))
;; A Daily-Stats is a (make-daily-stats Nat Str (listof Email-Record))
;; requires: each day-id in emails is unique

;;DEFINITIONS
(define stats1 (list (make-email-record 1 7 20)
                     (make-email-record 2 8 50)
                     (make-email-record 3 6 30)
                     (make-email-record 4 8 100)
                     (make-email-record 5 7 50)))

(define em1 (make-daily-stats 1 "Justin" empty))

(define em2 (make-daily-stats 2 "Tolu" stats1))

(define stats2 (list (make-email-record 1 10 80)
                     (make-email-record 2 5 30)
                     (make-email-record 3 7 30)
                     (make-email-record 4 7 80)
                     (make-email-record 5 12 80)))

(define em3 (make-daily-stats 10 "Liz" stats2))

(define em5 (make-daily-stats 5 "PASS" (list (make-email-record 1 7 25))))

(define em6 (make-daily-stats 69 "oop" (list (make-email-record 1 7 0)
                                             (make-email-record 2 7 2))))

(define em7 (make-daily-stats 7 "HYPE" (list (make-email-record 1 7 0)
                                             (make-email-record 2 5 0)
                                             (make-email-record 3 10 0))))
 

;;(avg-emails dailystats) takes in daily-stats structure dailystats
;; and produce the average email by each staff member.
;;
;; avg-emails: Daily-Stats -> Num
;;
;;Examples:
;(check-expect (avg-emails em7) 0)
;(check-expect (avg-emails em1) 0)

(define (avg-emails dailystats)
  
  (local [(define (total-emails loemails)
                   (cond [(empty? loemails) 0]
                         [else (+ (email-record-emails-sent
                                  (first loemails))
                                  (total-emails
                                  (rest loemails)))]))
         (define (total-hours loemails)
                   (cond [(empty? loemails) 0]
                         [else (+ (email-record-hours-worked
                                  (first loemails))
                                  (total-hours
                                  (rest loemails)))]))]

    (cond [(empty?  (daily-stats-emails dailystats)) 0]
          [else (/ (total-emails (daily-stats-emails dailystats))
                   (total-hours  (daily-stats-emails dailystats)))])))


;;TESTS:

;(check-expect (avg-emails em2) 250/36)
;(check-expect (avg-emails em6) 1/7)
;(check-expect (avg-emails em5) 25/7)
;(check-expect (avg-emails em3) 300/41)

;;(highest-email-record dailystats) takes in a Daily-Stats (dailystats)
;;   and produces the email record with max emails sent.
;;
;;highest-email-record: Daily-Stats -> (listof Email-Record)
;;
;;Examples:
(check-expect (highest-email-record em7)
              (list (make-email-record 1 7 0)
                    (make-email-record 2 5 0)
                    (make-email-record 3 10 0)))

(check-expect (highest-email-record em1) empty)

(define (highest-email-record dailystats)
  
  (local [(define (emails-sent loemails)
            
            (cond [(empty? loemails) 0]
                  [(> (email-record-emails-sent (first loemails))
                      (emails-sent (rest loemails)))
                   (email-record-emails-sent (first loemails))]
                  [else (emails-sent (rest loemails))]))
          
          (define (email-rec emails loemails)
            
            (cond [(empty? loemails) empty]
                  [(= (email-record-emails-sent (first loemails)) emails)
                   (cons (first loemails) (email-rec emails (rest loemails)))]
                  [else (email-rec emails (rest loemails))])) ]
             
  (cond [(empty? (daily-stats-emails dailystats)) empty]
        [else (email-rec (emails-sent (daily-stats-emails dailystats))
                         (daily-stats-emails dailystats))])))

;;TESTS:

(check-expect (highest-email-record em3)
              (list (make-email-record 1 10 80)
                    (make-email-record 4 7 80)
                    (make-email-record 5 12 80)))

(check-expect (highest-email-record em2)
              (list (make-email-record 4 8 100)))

(check-expect (highest-email-record em1) empty)

(check-expect (highest-email-record em6)
              (list (make-email-record 2 7 2)))























;;
               