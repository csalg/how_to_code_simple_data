#lang racket

;; A Racket version of the classic snake game
;; ========================================================================


;; Requires
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)



;; ========================================================================
;; Constants

;; MTS constants

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

;; Game attributes

(define REFRESH-RATE 7) ; Domain is Natural[1,). interp. The ticker is set at 28fps, this skips refresh cycles to make the game seem more retro.
(define SPEED 3) ; pixels/tick

;; Blocks
;; The physical appearance of the blocks are constants, only their positions get updated.

(define BLOCK-SIZE 20) ; Length of one of the sides of the block.
(define (block color) (rectangle BLOCK-SIZE BLOCK-SIZE "solid" color))
(define TARGET-BLOCK (block "red"))
(define SNAKE-BLOCK (block "green"))


;; ========================================================================
;; Data Definitions

;; Position -> Tuple
;; interp. Holds the xy-coordinates of blocks (with respect to MTS scene).
(define P1 (cons 10 10))
(define P2 (cons 50 50))
(define P3 (cons 500 500))
(define P4 (cons 50 450))
(define P5 (cons 150 50))
(define P6 (cons 170 50))
(define P7 (cons 190 50))

;; Game -> Data-struct
;; interp. Holds the positions of the snake blocks and target block:
;;		target is the position of the targets
;; 		snake is a list with the positions of all the snake blocks
(define-struct game (target snake direction))
(define GAME1 (make-game P1 (list P5 P6 P7) direction))

#;
(define (fn-for-game g)
  (... 	(fn-for-target (game-target g))
	  (cond [(empty? (game-snake g)) (...)]                   ;BASE CASE
	        [else (... (first (game-snake g))                 ;String
	                   (fn-for-snake (rest (game-snake g))))]))) ;NATURAL RECURSION

;; Template rules used:
;;  - compound: 2 fields
;;  - reference: ball field is Ball

;; ========================================================================
;; Functions

;; Game -> Image
;; interp. Renders the final scene with all the blocks overlaid

(define (render target snake scn)
  (cond [(not (boolean? target)) (render #f snake (place-image TARGET-BLOCK (car target) (cdr target) scn))]
        [(empty? snake) scn]                   ;BASE CASE
	    [else (render #f (rest snake) (place-image SNAKE-BLOCK (car (first snake)) (cdr (first snake)) scn))]))

;; (render (game-target GAME1) (game-snake GAME1) MTS)

;; List -> List
;; interp. Returns a list without the last element unless it's empty
(define (chop-tail lst)
  (cond [(or (empty? lst) (empty? (cdr lst))) empty]
        [else
         (cons (car lst) (chop-tail (cdr lst)))]))
  
(check-equal? (chop-tail (list 10 14 15)) (list 10 14))

;; Block, String -> Block
;; interp. Produces a new block translated in the direction specified in the second argument.

(define (update-block block direction)
  (cond 
    [(eq? direction "UP") (cons (car block) (+ (cdr block) SPEED))]
    [(eq? direction "DOWN") (cons (car block) (- (cdr block) SPEED))]
    [(eq? direction "RIGHT") (cons (+ (car block) SPEED) (cdr block))]
    [(eq? direction "LEFT") (cons (- (car block) SPEED) (cdr block))]
        ))

(check-equal? (update-block (cons 10 10) "UP") (cons 10 (+ 10 SPEED)))
(check-equal? (update-block (cons 10 10) "DOWN") (cons 10 (- 10 SPEED)))
(check-equal? (update-block (cons 10 10) "RIGHT") (cons (+ 10 SPEED) 10))
(check-equal? (update-block (cons 10 10) "LEFT") (cons (- 10 SPEED) 10))


;; Game -> Game
;; interp. Moves snake
(define (move-snake snake latest_block add_block)
  (cond [(eq? add_block #t) (cons latest_block snake)] ; if we are adding a new block, then we just add it to the beginning and we are done!
        [else (cons latest_block (chop-tail snake))] ; otherwise, we add the latest block but chop the last one
        ))

(check-equal? (move-snake (list (cons 170 150) (cons 150 150)) (cons 190 150) #t) (list (cons 190 150) (cons 170 150) (cons 150 150)))
(check-equal? (move-snake (list (cons 170 150) (cons 150 150)) (cons 190 150) #f) (list (cons 190 150) (cons 170 150)))
(move-snake (list (cons 170 150) (cons 150 150)) (update-block (cons 170 150) "UP") #f)

;; Block -> Block
;; interp. Returns a new block with a random position


