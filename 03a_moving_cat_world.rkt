#lang racket
(require 2htdp/image)	
(require 2htdp/universe)	

;; A cat that walks from left to right across the screen
;; =====================================================
;; Constants


(define WIDTH 600)
(define HEIGHT 400)

(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))

(define IMG (circle 90 "solid" "red"))

(define SPEED 1)
;; =====================================================
;; Data definitions

;; Circle is natural
;; interp. x-position of circle in screen coordinates.
;; Examples:

(define C1 0)            ; min value at left edge
(define C2 (/ WIDTH 2))  ; middle
(define C3 WIDTH)        ; max value at right edge

;; Function template:
#;
(define (fn-for-cat c)
  (... c))


;; Template rules used:
;;  - atomic non-distinct: Natural

;; =====================================================
;; Functions

;; Cat -> Cat
;;
;;
(define (main c)
  (big-bang c
    (on-tick advance-cat)
    (to-draw render)))

;; Cat -> Cat
;; Produces the next cat by advancing SPEED to the right.
;; !!!
(define (advance-cat c)
  (+ c SPEED))

(assert (advance-cat 4) (+ 4 SPEED))


;; Cat -> Image
;; Renders the image that will be updated on screen.
(define (render c)
  (place-image IMG c CTR-Y MTS))

(main C1)


