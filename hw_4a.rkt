;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hw_4a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ========================================================================================================================
;; Question 1: Self-Reference P2 - Double All
;; Design a function to double every number in a list.

; Number list -> Number list
; interp. Recursive function, produces a list with the values of the numbers passed doubled.
(define (double_my_list LON)
  (cond
       [(empty? LON) empty ]
       [else (cons (* 2 (first LON)) (double_my_list (rest LON)))]))

(check-expect (double_my_list (cons 10 (cons 5 (cons 8 empty)))) (list 20 10 16))


;; ========================================================================================================================
;; Question 2: Self-Reference P3 - Boolean List
;; Design a data definition to represent a list of booleans, and a function to determine if all values in a given list are true.

;; Data definition

;; ListOfBooleans is one of:
;;     - empty
;;     - (cons #t LB)
;;     - (cons #f LB)

(define LB1 (cons #t (cons #f (cons #f (cons #t empty)))))
(define LB2 (cons #t (cons #t empty)))

#;
(define (fn-for-lb lb)
  (cond [(empty? lb) (...)]                   ;BASE CASE
        [else (... (first lb)                 ;Do something with the first value
                   (fn-for-lb (rest lb)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Boolean LB)
;;  - self-reference: (rest LB) is ListOfBoolean

;; Function

;; LB -> Boolean
;; interp. returns #t if all elements in the list are true.
(check-expect (all_true? LB1) #f)
(check-expect (all_true? LB2) #t)
(check-expect (all_true? empty) #f)


(define (all_true? lb)
  (cond [(empty? lb) #f]
        [else (if
               (empty? (rest lb))
               (first lb)
               (and (first lb) (all_true? (rest lb)) ))])) 

;; ========================================================================================================================
;; Question 3: Self-Reference P5 - Largest
;; Design a function to find the largest number in a list of numbers.

;; LoN -> Number
;; interp. Produces the largest number in a list of numbers

(check-expect (largest? (list 4 10 5)) 10)
(check-expect (largest? (list -2 -5 -6)) -2)
(check-expect (largest? empty) empty)

;; THis algorithm could be optimised by storing (largest? (rest ln)), however 'Beginning Student' language doesn't seem to have 'begin' and scoped variables have not been mentioned in lectures.
(define (largest? ln)
  (cond [(empty? ln) empty]
        [else
               (if
               (empty? (rest ln))
               (first ln)
               (if (> (first ln) (largest? (rest ln))  )
               (first ln)
               (largest? (rest ln)) ))])) 
(largest? (list 10 20 5))

;; ========================================================================================================================
;; Question 4: Self-Reference P6 - Image List
;; Design a data definition to represent a list of images, and a function to find the sum of areas from a list of images.

;; More of the same... zzz...