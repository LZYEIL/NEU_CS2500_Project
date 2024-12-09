;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw6_Pipe-Fantasyv2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; PART 1 TASK1:!!!

(define-struct pipe [top bot left right])

;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TB-LR (make-pipe #true #true #true #true))



(define (pipe-temp pipes)
  (...
   (pipe-top pipes)...
   (pipe-bot pipes)...
   (pipe-left pipes)...
   (pipe-right pipes)...))




;; PART 1 TASK2:!!!



(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-TB-LR))






;; PART 1 TASK3:!!!

;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length

(define (pipe->image pipe tile-side-length pipe-width)
  (if
   (> pipe-width tile-side-length)
   (square 1 "solid" "transparent")
   (overlay/align "middle" "top" (rectangle pipe-width (/ tile-side-length 2) "solid"
                                            (if (pipe-top pipe) "black" "transparent"))
                  (overlay/align "middle" "bottom" (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                              (if (pipe-bot pipe) "black" "transparent"))
                                 (overlay/align "left" "middle" (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                           (if (pipe-left pipe) "black" "transparent"))
                                                (overlay/align "right" "middle" (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                           (if (pipe-right pipe) "black" "transparent"))
                                                               (overlay (square pipe-width "solid" "black")
                                                                        (square tile-side-length "solid" "Medium Gray"))))))))

;; PART 2 TASK4:!!!

; Complete a data design called Grid that represents a grid.
; You should construct several examples of varying sizes
; and with different pipes places on them.

; Recommendation 2: A better approach is to only represent the list of placed pipes.
; So, a blank grid would be represented by the empty list.
; Thus each item in the list must represent both a pipe and its coordinates on the grid.
; (This requires an auxiliary data definition for a pipe with its coordinates.)

(define-struct pipeNew [pipes x y])
; A PipeNew is a (make-pipeNew Pipe Number Number)
; Interpretation: A pipe with its x/y coordinates

(define PIPE-NEW-1 (make-pipeNew PIPE-TL 3 3))
(define PIPE-NEW-2 (make-pipeNew PIPE-BL 5 5))
(define PIPE-NEW-3 (make-pipeNew PIPE-BR 7 7))
(define PIPE-NEW-4 (make-pipeNew PIPE-TB-LR 9 9))

(define (pipeNew-temp ps)
  (...
   (pipe-temp (pipeNew-pipes ps))...
   (pipeNew-x ps)...
   (pipeNew-y ps)...))


; A [List-of PipeNew] (LoP) is one of:
; - '()
; -  (cons PipeNew LoP)

; A list of PipeNew elements

(define LOP-1 '())
(define LOP-2 (cons PIPE-NEW-1 LOP-1))
(define LOP-3 (cons PIPE-NEW-2 LOP-2))

(define (lop-temp lp)
  (...
   (cond
     [(empty? lp)...]
     [(cons? lp)
      (pipeNew-temp (first lp))...
      (lop-temp (rest lp))...])))
   


(define-struct grid [dim lop])
; A Grid is a (make-grid Number [List-of PipeNew])
; Interpretation: A grid with dimensions and PipeNew elements


(define GRID-1 (make-grid 7 LOP-1))
(define GRID-2 (make-grid 9 LOP-2))
(define GRID-3 (make-grid 9 LOP-3))


(define (grid-temp gd)
  (...
   (grid-dim gd)...
   (lop-temp (grid-lop gd))...))
  


;; PART 2 TASK5:!!!

; Create an example that represents an empty 7 × 7 grid called STARTING-GRID


(define STARTING-GRID (make-grid 7 '()))



;; PART 2 TASK6:!!!

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.

(check-expect (place-pipe STARTING-GRID PIPE-TL 3 3)
              (make-grid 7 (list (make-pipeNew (make-pipe #true #false #true #false) 3 3))))


(check-expect (place-pipe GRID-2 PIPE-TB 2 2)
              (make-grid 9 (list (make-pipeNew (make-pipe #true #true #false #false) 2 2)
                                 (make-pipeNew (make-pipe #true #false #true #false) 3 3))))

(check-expect (place-pipe GRID-3 PIPE-TR 1 1)
              (make-grid 9 (list (make-pipeNew (make-pipe #true #false #false #true) 1 1)
                                 (make-pipeNew (make-pipe #false #true #true #false) 5 5)
                                 (make-pipeNew (make-pipe #true #false #true #false) 3 3))))
              

  
              

(define (place-pipe grid pipe row col)
  (make-grid (grid-dim grid)
             (cons (make-pipeNew pipe col row) (grid-lop grid))))





;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.


(check-expect (pipe-at GRID-1 1 1) #false)
(check-expect (pipe-at GRID-2 3 3) (make-pipe #true #false #true #false))
(check-expect (pipe-at GRID-3 5 5) (make-pipe #false #true #true #false))

(define (pipe-at grid row col)
  (local
    [(define (is-pipe? ps)
       (and (= (pipeNew-x ps) col) (= (pipeNew-y ps) row)))
     (define my-filter (filter is-pipe? (grid-lop grid)))]
    (if (empty? my-filter)
        #false
        (pipeNew-pipes (first my-filter)))))





; Without list abstractions: 
#;(define (lop-actions lop row col)
    (cond
      [(empty? lop) #false]
      [(cons? lop)
       (if
        (and (= (pipeNew-x (first lop)) col)
             (= (pipeNew-y (first lop)) row))
        (pipeNew-pipes (first lop))
        (lop-actions (rest lop) row col))]))

#;(define (pipe-at grid row col)
    (lop-actions (grid-lop grid) row col))


  

;; PART 2 TASK7:!!!

; Complete the following function design.
; Do not modify its signature, and you do not need to write check-expects for it


;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.

; create-blank-row: Number Integer -> Image
; Draws the n beside empty squares

(define (create-blank-row n tile-side-length)
  (local
    [(define (my-row-list n tile-side-length) 
       (cond
         [(= n 0) '()]
         [else
          (cons (square tile-side-length "outline" "black")
                (my-row-list (- n 1) tile-side-length))]))]
    (foldr beside (square 1 "solid" "transparent") (my-row-list n tile-side-length))))




; create-blank-grid: Number Integer -> Image
; Draws the n*n empty grid image
(define (create-blank-grid n tile-side-length)
  (local
    [(define row-size n) 
     (define (my-grid-list n tile-side-length) 
       (cond
         [(= n 0) '()]
         [else
          (cons (create-blank-row row-size tile-side-length)  
                (my-grid-list (- n 1) tile-side-length))]))]
    (foldr above (square 1 "solid" "transparent") (my-grid-list n tile-side-length))))








;lop->image: [List-of PipeNew] Integer Integer Image --> Image
; draws the image of grid with list of pipeNew
(define (lop->image lop tile-side-length pipe-width imag)
  (cond
    [(empty? lop) imag]
    [(cons? lop) (place-image
                  (pipe->image (pipeNew-pipes (first lop)) tile-side-length pipe-width)
                  (+ (* (pipeNew-x (first lop)) tile-side-length) (/ tile-side-length 2))
                  (+ (* (pipeNew-y (first lop)) tile-side-length) (/ tile-side-length 2))
                  (lop->image (rest lop) tile-side-length pipe-width imag))]))

; THE SECOND VERSION:
#;(define (lop->image lop tile-side-length pipe-width imag)
    (cond
      [(empty? lop) imag]
      [(cons? lop)
       (place-image/align (pipe->image (pipeNew-pipes (first lop)) tile-side-length pipe-width)
                          (* (pipeNew-x (first lop)) tile-side-length)
                          (* (pipeNew-y (first lop)) tile-side-length)
                          "left"
                          "top"
                          imag)]))
                         
         

(define (grid->image grid tile-side-length pipe-width)
  (lop->image (grid-lop grid) tile-side-length pipe-width (create-blank-grid (grid-dim grid) tile-side-length)))




;; PART 3 TASK8:!!!

; Complete a data design called GameState. For now, a game should have a grid and a list of “incoming pipes”
; that may appear when the player clicks on a grid cell. In a complete game, the pipes are generated randomly
; and there is an infinite supply of pipes. However, for now, you’ll have a fixed list of incoming pipes.


(define FIXED-INCOMING-PIPES (list PIPE-TL PIPE-LR PIPE-TB PIPE-TB-LR PIPE-BR))

;; A [List-Of Pipe] is one of:
;; '()
;; (cons Pipe [List-Of Pipe])

(define LOPS-1 '())
(define LOPS-2 (cons PIPE-TL LOPS-1))
(define LOPS-3 (cons PIPE-TR LOPS-2))


(define (lops-temp lops)
  (...
   (cond
     [(empty? lops) ...]
     [(cons? lops) ...
      (pipe-temp (first lops)) ...
      (lops-temp (rest lops)) ...])))

(define-struct gamestate [grid lops])

; A Gamestate is (make-gamestate Grid [List-of Pipe])
; Interpretation: a Gamestate with background of a n*n grid and list of incoming pipes

(define GAMESTATE-1 (make-gamestate GRID-1 LOPS-2))
(define GAMESTATE-2 (make-gamestate GRID-2 LOPS-2))
(define GAMESTATE-3 (make-gamestate GRID-3 FIXED-INCOMING-PIPES))


(define (gamestate-temp ga)
  (... (grid-temp (gamestate-grid ga)) ...
       (lop-temp (gamestate-lop ga)) ...))




;; PART 3 TASK9:!!!

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.

(define TSL 50)
(define PIPE-WIDTH 30)


(check-expect (place-pipe-on-click GAMESTATE-1 800 600 "button-up")
              (make-gamestate (make-grid 7 (list (make-pipeNew (make-pipe #true #false #true #false) 16 12))) '()))

(check-expect (place-pipe-on-click GAMESTATE-2 400 200 "button-up")
              (make-gamestate (make-grid 9 (list (make-pipeNew (make-pipe #true #false #true #false) 8 4)
                                                 (make-pipeNew (make-pipe #true #false #true #false) 3 3))) '()))

(check-expect (place-pipe-on-click GAMESTATE-3 500 100 "button-up")
              (make-gamestate (make-grid 9 (list
                                            (make-pipeNew (make-pipe #true #false #true #false) 10 2)
                                            (make-pipeNew (make-pipe #false #true #true #false) 5 5)
                                            (make-pipeNew (make-pipe #true #false #true #false) 3 3)))
                              (list
                               (make-pipe #false #false #true #true)
                               (make-pipe #true #true #false #false)
                               (make-pipe #true #true #true #true)
                               (make-pipe #false #true #false #true))))



(define (place-pipe-on-click initial-game-state x y me)
  (if
   (mouse=? me "button-up")
   (make-gamestate
    (place-pipe (gamestate-grid initial-game-state) (first (gamestate-lops initial-game-state)) (floor (/ y TSL)) (floor (/ x TSL)))
    (rest (gamestate-lops initial-game-state)))
   initial-game-state))







;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    (to-draw draw-image)
    (on-mouse place-pipe-on-click)))

; 
(define (draw-image initial-game-state)
  (grid->image
   (gamestate-grid initial-game-state)
   TSL
   PIPE-WIDTH))


;(pipe-fantasy GAMESTATE-3)
