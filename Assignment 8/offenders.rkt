;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname offenders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 08, Problem 3
;;***************************************************
;;

;;PART A---------------------------------------------------------------

(define-struct email (staff-id id-list word-count reply))
;;An Email is a (make-email Nat (listof Nat) Nat (listof Email))
;;requires word-count >= 0

;;DEFINITIONS --------------------------------------------------------

(define email10 (make-email 2 (list 4 7) 5 empty))
(define email9 (make-email 7 empty 3 empty))
(define email8 (make-email 9 (list 4 7) 0 (list email10 email9)))
(define email13 (make-email 9 (list 4 7) 0 empty))
(define email7 (make-email 1 (list 2 3) 5 empty))
(define email14 (make-email 1 (list 2 3) 1 empty))
(define email6 (make-email 2 (list 1 4 5) 10 (list email7)))
(define email5 (make-email 3 (list 6 7) 15 (list email6)))
(define email4 (make-email 1 (list 2 3 4 5 6 7) 20 empty))
(define email11 (make-email 2 (list 4 7) 5 (list email4 email5)))
(define email3 (make-email 2 (list 3 5 6) 25 empty))
(define email2 (make-email 3 (list 4 7) 30 empty))
(define email1 (make-email 1 (list 2 3 4 5 6 7) 35(list email2 email3 email4)))
(define email12 (make-email 2 (list 4 7) 5(list email1 email2 email3 email4)))


;;PART B---------------------------------------------------------------

;;(total-word-count em) takes in an email(em) and returns the total word count
;;   of the email and it's replies.
;;
;;total-word-count: Email -> Nat 
;;
;;Examples:
(check-expect (total-word-count email1) 110)
(check-expect (total-word-count email2) 30)

(define (total-word-count em)
  
  (local [;;(word-count ems) returns the total word count of all emails in the
          ;;   list ems!
          ;;word-count (listof Email) -> Nat 
          (define (word-count ems)
            (cond [(empty? ems) 0]
                  [(not (cons? ems))
                   (email-word-count ems)]
                  [else(+ (email-word-count (first ems))
                          (word-count (rest ems))
                          (word-count (email-reply (first ems))))]))]
    
    (+ (word-count em)
       (word-count (email-reply em)))))


;;TESTS:
(check-expect (total-word-count email3) 25)
(check-expect (total-word-count email4) 20)
(check-expect (total-word-count email5) 30)
(check-expect (total-word-count email6) 15)
(check-expect (total-word-count email13) 0)
(check-expect (total-word-count email9)  3)
(check-expect (total-word-count email10) 5)
(check-expect (total-word-count email12) 190)
(check-expect (total-word-count email11) 55)
(check-expect (total-word-count email14) 1)

;;PART C ---------------------------------------------------------------

;;(unique-email-senders loemails) takes in a list of emails (loemails) and
;;    produces a list of unique sender id's.
;;
;;unique-email-senders: (listof Email) -> (listof Nat)
;;
;;Examples:
(check-expect (unique-email-senders (list email1)) (list 3 2 1))

(check-expect (unique-email-senders (list email7)) (list 1))


(define (unique-email-senders loemails)
  
  (local [;;(in-list sender losender) predicate functions which tells us
          ;;   if sender is already in the list-losender or not.
          ;;in-list: Email (listof Nat) -> (anyof True False)
          (define (in-list sender losender)
            (cond [(empty? losender) false]
                  [(= sender (first losender)) true]
                  [else (in-list sender (rest losender))]))

          ;;(new-list losender) forms a new list and adds elements if they
          ;; are not already in the list-losender.
          ;;new-list: (listof Nat) -> (listof Nat)
          (define (new-list losender)
            (cond [(empty? losender) empty]
                  [(not(in-list (first losender) (rest losender)))
                   (cons (first losender) (new-list (rest losender)))]
                  [else (new-list (rest losender))]))

          ;;(output-list loemails) forms a new list and adds the staff-id of
          ;;   of all emails and their replies to the list from loemails.
          ;;new-list: (listof Email) -> (listof Nat)
          (define (output-list loemails)           
            (cond [(empty? loemails) empty]
                  [else (append (list(email-staff-id (first loemails)))
                   (output-list (email-reply (first loemails)))
                   (output-list (rest loemails)))]))]

    (new-list (output-list loemails))))

;;Tests:
(check-expect (unique-email-senders (list (make-email 3 (list 4 7) 30 empty)
                            (make-email 2 (list 3 5 6) 25 empty)))
                      (list 3 2))
(check-expect (unique-email-senders (list email1 email7 email2 email3))
              (list 1 3 2))

(check-expect (unique-email-senders (list email7)) (list 1))

(check-expect (unique-email-senders (list email8 email9)) (list 9 2 7))

(check-expect(unique-email-senders (list email12 email6 email7 email14 email9))
              (list 3 2 1 7))


;;PART D ---------------------------------------------------------------


;;(sent-email-summary loemails) takes in a loemails list of emails and
;;   produces each unique email with the number of emails sent.
;;
;;sent-email-summary: (listof Email) -> (listof (listof Nat Nat))
;;
;;Examples:
(check-expect (sent-email-summary (list email2)) (list (list 3 1)))
(check-expect (sent-email-summary (list email8))
              (list (list 9 1) (list 2 1) (list 7 1)))

(define (sent-email-summary loemails)

  (local [;;(in-list sender losender) predicate functions which tells us
          ;;if sender is already in the list-losender or not.
          ;;in-list: Email (listof Nat) -> (anyof True False)
          (define (in-list sender losender)
            (cond [(empty? losender) false]
                  [(= sender (first losender)) true]
                  [else (in-list sender (rest losender))]))

          ;;(new-list losender) forms a new list and adds elements if they
          ;; are not already in the list-losender.
          ;;new-list: (listof Nat) -> (listof Nat)
          (define (new-list losender)
            (cond [(empty? losender) empty]
                  [(not(in-list (first losender) (rest losender)))
                   (cons (list (first losender) (no-of-ins (first losender)
                                                   (output-list loemails)))
                         (new-list (rest losender)))]
                  [else (new-list (rest losender))]))

          ;;(no-of-ins num losender) counts the instances of staff-id's
          ;; in list-losender sending emails.
          ;;no-of-ins: (listof Nat) -> Nat
          (define (no-of-ins num losender)
            (cond [(empty? losender) 0]
                  [(= num (first losender))
                   (+ 1 (no-of-ins num (rest losender)))]
                  [else (+ 0 (no-of-ins num (rest losender)))]))
            
          ;;(output-list loemails) forms a new list and adds the staff-id of
          ;; of all emails from loemails and their replies to a list.
          ;;new-list: (listof Email) -> (listof Nat)
          (define (output-list loemails)
            (cond [(empty? loemails) empty]
                  [else (append (list (email-staff-id (first loemails)))
                   (output-list (email-reply (first loemails)))
                   (output-list (rest loemails)))]))]

   (new-list (output-list loemails))))

;;Tests:
(check-expect (sent-email-summary (list email1))
              (list (list 3 1) (list 2 1) (list 1 2)))

(check-expect (sent-email-summary (list email1 email2 email3 email12))
               (list (list 3 4) (list 2 5) (list 1 5)))

(check-expect (sent-email-summary (list email1))
              (list (list 3 1) (list 2 1) (list 1 2)))

(check-expect (sent-email-summary (list email1 email12 email7))
(list (list 3 3) (list 2 4) (list 1 6)))

(check-expect (sent-email-summary (list email10))
(list (list 2 1)))

(check-expect (sent-email-summary (list email10 email1 email9 email13))
(list (list 3 1) (list 2 2) (list 1 2) (list 7 1) (list 9 1)))

(check-expect (sent-email-summary empty) empty)


;;PART E ---------------------------------------------------------------


;;(email-offenders loemails thresh) processes on a list of emails (loemails)
;;    and returns offenders who've sent more than 'thresh' no of emails.
;;
;;email-offenders: (listof Email) Nat -> (listof Nat)
;;
;;Examples:
(check-expect (email-offenders (list email1) 1) (list 1))        
(check-expect (email-offenders (list email1) 3) empty)

(define (email-offenders loemails thresh)
  
  (local [;;(offenders lounique thresh) produces a list-lounique of 
          ;; all staff's who've sent more than 'thresh' no of emails.
          ;;offenders: (listof Nat) Nat -> (listof Nat)
          (define (offenders lounique thresh)
            (cond [(empty? lounique) empty]
                  [(> (second (first lounique)) thresh)
                   (cons (first (first lounique))
                         (offenders (rest lounique) thresh))]
                  [else (offenders (rest lounique) thresh)]))]

    (sort (offenders (sent-email-summary loemails) thresh) <)))

;;Tests:
(check-expect (email-offenders (list email1 email2 email12) 1) (list 1 2 3))        
(check-expect (email-offenders (list email8 email9) 3) empty)
(check-expect (email-offenders (list email8 email9) 1) (list 7))
(check-expect (email-offenders (list email8 email9) 0) (list 2 7 9))
(check-expect (email-offenders empty 0) empty)
(check-expect (email-offenders (list email1 email12 email7 email8 email9) 5)
              (list 1))
(check-expect (email-offenders (list email1 email12 email7 email8 email9) 6)
              empty)
(check-expect(email-offenders (list email1 email2 email3 email4 email5 email6 
                       email10 email11 email13 email12 email7 email8 email9) 0)
(list 1 2 3 7 9))
(check-expect(email-offenders (list email1 email2 email3 email4 email5 email6
                       email10 email11 email13 email12 email7 email8 email9) 5)
(list 1 2 3))
(check-expect(email-offenders (list email1 email2 email3 email4 email5 email6
                 email10 email11 email13 email12 email7 email8 email9) 7)
(list 1 2))
(check-expect(email-offenders (list email1 email2 email3 email4 email5 email6 
                     email10 email11 email13 email12 email7 email8 email9) 12)
empty)


















                   




                                  
                                   