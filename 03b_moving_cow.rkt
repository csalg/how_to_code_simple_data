;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 03b_moving_cow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;; A cow that moves from left to right.
;; ==============================================================
;; Constants

;; Width -> Number
;; Width of the MTS
(define WIDTH 600)

;; Height -> Number
;; Height of MTS
(define HEIGHT 400)

;; Center -> Number
;; Center of MTS
(define CTR-Y (/ HEIGHT 2))

;; Cow -> Circle
;; Images seem to crash the interpretter so we are using a circle.
(define LCOW (circle 10 "solid" "red"))
(define RCOW (circle 10 "solid" "blue"))

;; Speed -> Number
;; interp. Pixel change in cow_position per ticker
(define SPEED 5)

;; Canvas -> MTS
;; Canvas objects which the course calls MTS
(define MTS (empty-scene WIDTH HEIGHT))

;; ==============================================================
;; Data definitions

(define-struct cow (x dx))
;; cow is (make-cow Natural[0,WIDTH] Integer)
;; interp. (make-cow x dx) is a cow with x coordinates and velocity dx.
;;     x is the centre of the cow.
;;     x is in pixels.
;;     dx is in pixels per tick.
(define C1 (make-cow 10 10))
(define C2 (make-cow 10 -5))

;; ==============================================================
;; Functions

(define (render c)
  c)

(define (move c)
  (make-cow
   (+ (cow-x c) (cow-dx c))
   (cow-dx c)))

(define (change-direction c)
  (... c))

(define (main c)
  (big-bang c
    (on-tick move)
    (on-key change-direction)
    (to-draw render)))



