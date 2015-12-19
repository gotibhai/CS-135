;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wonderdiet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;;Pushkin Abbott (#20620798)
;;CS 135 Fall 2015
;;Assignment 02, Problem 4
;;***************************************************

(define-struct nutri-info (serving-size fat carb prot))
;; A nutri-info is a (make-nutri-info Num Num Num Num)
;; requires serving-size >= 0
;;          fat >= 0
;;          carb >= 0
;;          prot >= 0

;;(valid-nutri-info? value) tells if the nutri-info struct is valid or not!
;;   
;;valid-nutri-info?: Nutri-Info -> bool 
;;
;;Examples:
(check-expect (valid-nutri-info? (make-nutri-info 30 10 10 10) )true)
(check-expect (valid-nutri-info? (make-nutri-info 20 10 10 10) )false)

(define (valid-nutri-info? value)
  
  (and (nutri-info? value)
       (>= (nutri-info-serving-size value)0)
       (>= (nutri-info-prot value) 0)
       (>= (nutri-info-fat value) 0)
       (>= (nutri-info-carb value) 0)
       (>= (nutri-info-serving-size value)
                               (+ (nutri-info-fat value)
                                  (nutri-info-carb value)
                                  (nutri-info-prot value)))))

;;Tests:
(check-expect (valid-nutri-info? (make-nutri-info 50 10 10 10))  true)
(check-expect (valid-nutri-info? (make-nutri-info 0 0 0 0)) true)
(check-expect (valid-nutri-info? (make-nutri-info -10 10 10 10)) false)
(check-expect (valid-nutri-info? (make-nutri-info 10 -10 -10 -10)) false)
(check-expect (valid-nutri-info? (make-nutri-info 40 -50 50 30)) false)
(check-expect (valid-nutri-info? (make-nutri-info 20 0 10 10)) true)
(check-expect (valid-nutri-info? (make-nutri-info 30 10 0 10)) true)
(check-expect (valid-nutri-info? (make-nutri-info 30 10 10 0)) true)
(check-expect (valid-nutri-info? (make-nutri-info 30 0 0 0)) true)



;;***************************************************

;;(higher-protein value1 value2) returns the struct with the higher protein
;; value. If the protein value is equal it returns the struct with smaller serving size.
;; If the serving size is same, it just returns the first structure! 
;;
;;higher-protein: Nutri-Info Nutri-Info -> Nutri-Info
;;
;;Examples:
(check-expect (higher-protein (make-nutri-info 40 10 10 20)
                              (make-nutri-info 45 10 10 25))
                              (make-nutri-info 45 10 10 25))

(check-expect (higher-protein (make-nutri-info 40 10 10 20)
                              (make-nutri-info 35 10 10 15))
                              (make-nutri-info 40 10 10 20))

(define (higher-protein value1 value2)
  (cond [(>(nutri-info-prot value1) (nutri-info-prot value2)) value1]  
        [(<(nutri-info-prot value1) (nutri-info-prot value2)) value2]
        [(and (= (nutri-info-prot value1) (nutri-info-prot value2))
              (<(nutri-info-serving-size value1)
                (nutri-info-serving-size value2))) value1]
        [(and (= (nutri-info-prot value1) (nutri-info-prot value2))
              (>(nutri-info-serving-size value1)
                (nutri-info-serving-size value2))) value2]
        [(and(= (nutri-info-prot value1) (nutri-info-prot value2))
             (= (nutri-info-serving-size value1)
                (nutri-info-serving-size value2))) value1]))

;;Tests
(define nutri-info-1 (make-nutri-info 50 20 10 20))
(define nutri-info-2 (make-nutri-info 40 10 10 20))
(define nutri-info-3 (make-nutri-info 40 10 0 20))


(check-expect (higher-protein nutri-info-1 nutri-info-2) nutri-info-2)
(check-expect (higher-protein nutri-info-2 nutri-info-1) nutri-info-2)
(check-expect (higher-protein nutri-info-2 nutri-info-3) nutri-info-2)
(check-expect (higher-protein nutri-info-1 nutri-info-3) nutri-info-3)

;;***************************************************

;;(combine-nutri-info value1 value2) takes in 2 structures of the type Nutri-info
;; and combines the value of their parameters. 
;; 
;;combine-nutri-info: Nutri-Info Nutri-Info -> Nutri-Info
;;
;;Examples:
(check-expect(combine-nutri-info (make-nutri-info 30 10 10 10)
                                 (make-nutri-info 60 20 20 20))
                                 (make-nutri-info 90 30 30 30))
(check-expect(combine-nutri-info (make-nutri-info 60 50  5  5)
                                 (make-nutri-info 60 20 20 20))
                                 (make-nutri-info 120 70 25 25))

(define (combine-nutri-info value1 value2)
  (make-nutri-info
   (+ (nutri-info-serving-size value1) (nutri-info-serving-size value2))
   (+ (nutri-info-fat value1) (nutri-info-fat value2))
   (+ (nutri-info-carb value1) (nutri-info-carb value2))
   (+ (nutri-info-prot value1) (nutri-info-prot value2))))

;;Tests
(define value1 (make-nutri-info 30 10 10 10))
(define value2 (make-nutri-info 70 50 10 10))
(define value3 (make-nutri-info 100 60 20 20))

(check-expect(combine-nutri-info value1 value2) value3)
(check-expect(combine-nutri-info value1 value3) (make-nutri-info 130 70 30 30))
(check-expect(combine-nutri-info value2 value3) (make-nutri-info 170 110 30 30))

;;***************************************************

;;(good-combo? value1 value2) is a function which takes in two nutri-info structures
;; and checks if your second meal fits into the nutrition requirements.
;;
;;good-combo?: Nutri-Info Nutri-Info -> bool
;;
;; Examples
(check-expect(good-combo? value1 value2) false)
(check-expect(good-combo? value2 value3) false)

(define nutri-limit 5)

(define (good-combo? nutri1 nutri2)
  (and (<= (abs(-(nutri-info-fat nutri1)
                 (nutri-info-fat nutri2)))   nutri-limit)
       (<= (abs(-(nutri-info-carb nutri1)
                 (nutri-info-carb nutri2)))  nutri-limit)
       (<= (abs(-(nutri-info-prot nutri1)
                 (nutri-info-prot nutri2)))  nutri-limit)))

(define value4 (make-nutri-info 30 10 10 10))
(define value6 (make-nutri-info 30 11 16 19))
(define value7 (make-nutri-info 30 12 13 19))
(define value8 (make-nutri-info 30 10 12 13))
(define value10 (make-nutri-info 21 18 8 17))
(define value11 (make-nutri-info 21 18 3 12))
(define value12 (make-nutri-info 35 15 15 15))

;;Tests
(check-expect (good-combo? value4 value6)  false)
(check-expect (good-combo? value4 value7)  false)
(check-expect (good-combo? value4 value8)   true)
(check-expect (good-combo? value4 value10) false)
(check-expect (good-combo? value4 value11) false)
(check-expect (good-combo? value4 value12)  true)

;;***************************************************





         
