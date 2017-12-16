#lang racket

;; A Racket version of the classic snake game. Nothing too fancy!

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

(define REFRESH-RATE 2) ; Domain is Natural[1,). interp. The ticker is set at 28fps, this skips refresh cycles to make the game seem more retro.
;; The speed is one block per refresh, so bear that in mind when setting the refresh rate.

;; Blocks
;; The physical appearance of the blocks.

(define BLOCK-SIZE 20) ; Length of one of the sides of the block.
(define SPEED BLOCK-SIZE)
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
(define P7 (cons 0 20))
(define P8 (cons 20 20))
(define P9 (cons 40 20))
(define P10 (cons 60 20))

;; Game -> Data-struct
;; interp. Holds the positions of the snake blocks and target block:
;;		target is the position of the targets
;; 		snake is a list with the positions of all the snake blocks
(define-struct game (target snake direction refresh next-block scene))
(define GAME1 (make-game (cons (/ WIDTH 2) (/ HEIGHT 2)) (list P7 P8 P9) "LEFT" 0 P10 MTS))

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

(define (render g)
  (define (render_iter target snake scn)
	  (cond [(not (boolean? target)) (render_iter #f snake (place-image TARGET-BLOCK (car target) (cdr target) scn))]
	        [(empty? snake) scn]
		    [else (render_iter #f (rest snake) (place-image SNAKE-BLOCK (car (first snake)) (cdr (first snake)) scn))]))
  (render_iter (game-target g) (game-snake g) (game-scene g)))

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
    [(eq? direction "UP") (cons (car block) (- (cdr block) SPEED))]
    [(eq? direction "DOWN") (cons (car block) (+ (cdr block) SPEED))]
    [(eq? direction "LEFT") (cons (- (car block) SPEED) (cdr block))]
    [(eq? direction "RIGHT") (cons (+ (car block) SPEED) (cdr block))]
 ))


(check-equal? (update-block (cons 10 10) "UP") (cons 10 (- 10 SPEED)))
(check-equal? (update-block (cons 10 10) "DOWN") (cons 10 (+ 10 SPEED)))
(check-equal? (update-block (cons 10 10) "LEFT") (cons (- 10 SPEED) 10))
(check-equal? (update-block (cons 10 10) "RIGHT") (cons (+ 10 SPEED) 10))

;; Snake, Target -> Boolean
;; interp. Returns #t if snake is close enough to the target

(define (on-target? snake target)
  (and (<= (abs (- (car (first snake)) (car target))) (/ BLOCK-SIZE 2))
      (<= (abs (- (cdr (first snake)) (cdr target))) (/ BLOCK-SIZE 2))))

;; Snake -> Boolean
;; interp. Assesses whether the snake has hit a wall

(define (hit-walls? snake)
  (define snk (first snake))
  (or
   (< (car snk) 0)
   (> (car snk) WIDTH)
   (< (cdr snk) 0)
   (> (cdr snk) HEIGHT)))

;; Game -> Game
;; interp. Moves snake

(define (move g)
  (define (move-snake snake latest_block add_block)
    (cond [(eq? add_block #t) (cons latest_block snake)] ; if we are adding a new block, then we just add it to the beginning and we are done!
          [else (cons latest_block (chop-tail snake))] ; otherwise, we add the latest block but chop the last one
          ))
  ;; Since the helper function is scoped to the move function the tests go inside.
  (check-equal? (move-snake (list (cons 170 150) (cons 150 150)) (cons 190 150) #t) (list (cons 190 150) (cons 170 150) (cons 150 150)))
  (check-equal? (move-snake (list (cons 170 150) (cons 150 150)) (cons 190 150) #f) (list (cons 190 150) (cons 170 150)))

  (define hit_target (on-target? (game-snake g) (game-target g)))
  (if
   (= (game-refresh g) 0)
   (cond [(hit-walls? (game-snake g)) GAME-START]
   [else (struct-copy game g
                [refresh REFRESH-RATE]
                [snake (move-snake 
                        (game-snake g) 
                        (update-block (car (game-snake g)) (game-direction g)) 
                        hit_target)]
                [target (if
                          hit_target
                          (random-block)
                          (game-target g)
                          )])])
    (struct-copy game g [refresh (- (game-refresh g) 1)])))

;; Game, key -> Game
;; interp. Changes the direction in the state.

(define (change-direction g key)
  (cond
     [(string=? key "up") (struct-copy game g [direction "UP"])]
     [(string=? key "down") (struct-copy game g [direction "DOWN"])]
     [(string=? key "right") (struct-copy game g [direction "RIGHT"])]
     [(string=? key "left") (struct-copy game g [direction "LEFT"])]
     [else (begin
             (display key)
             g)]))
  
;;  -> Block
;; interp. Returns a new block with a random position

(define (random-block)
  (cons (random WIDTH) (random HEIGHT)))
  
;; Game -> Big-bang
;; interp. Main function which starts the game.

(define (main g)
  (big-bang g
    (to-draw render)
    (on-tick move)
    (on-key change-direction)
    ))

(define P11 (cons 200 400))
(define P12 (cons 220 400))
(define P13 (cons 240 400))
(define P14 (cons 260 400))

(define GAME-START (make-game (cons (/ WIDTH 2) (/ HEIGHT 2)) (list P11 P12 P13) "RIGHT" 0 P14 MTS))
(main GAME-START)
  
  
  
  
  
  
  
  
  
  
  