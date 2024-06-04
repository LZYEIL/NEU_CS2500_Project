;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FINAL_PIPE_FANTASY) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; HW 11 TASK7: THE FINAL GAME

#;(define YOUR-INCOMING-PIPES (list PIPE-BL PIPE-TB-LR PIPE-TB PIPE-TL PIPE-TR PIPE-TB-LR PIPE-BR
                                  PIPE-LR PIPE-TL PIPE-BR PIPE-TB PIPE-LR PIPE-TB-LR PIPE-TL))

;(define YOUR-REQUIRED-GAMESTATE (gamestate-init 6 1 1 "right" YOUR-INCOMING-PIPES))
;(pipe-fantasy YOUR-REQUIRED-GAMESTATE)


;YOU SHOULD USE THESE ONES ABOVE





(define-struct pipe [top bot left right starting?])

; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean Boolean)

; Interpretation: a pipe with openings in the given directions. A #true for 
; one of top, bot, left, right indicates an opening in that direction.
; The last Boolean indicates whether the pipe is a starting pipe.

(define PIPE-TL (make-pipe #true #false #true #false #false))
(define PIPE-TR (make-pipe #true #false #false #true #false))
(define PIPE-BL (make-pipe #false #true #true #false #false))
(define PIPE-BR (make-pipe #false #true #false #true #false))
(define PIPE-TB (make-pipe #true #true #false #false #false))
(define PIPE-LR (make-pipe #false #false #true #true #false))
(define PIPE-TB-LR (make-pipe #true #true #true #true #false))

(define PIPE-S1 (make-pipe #true #false #false #false #true))
(define PIPE-S2 (make-pipe #false #true #false #false #true))
(define PIPE-S3 (make-pipe #false #false #true #false #true))
(define PIPE-S4 (make-pipe #false #false #false #true #true))


(define (pipe-temp pipes)
  (...
   (pipe-top pipes)...
   (pipe-bot pipes)...
   (pipe-left pipes)...
   (pipe-right pipes)...
   (pipe-starting? pipes)...))


(define TSL 100)
(define PIPE-WIDTH 30)



; (HW11 TASK 1)

;; pipe->image: Pipe Integer Integer Boolean Directions-> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length

(define (pipe->image pipe tile-side-length pipe-width filled? dir)
  (local
    [(define GOO-COLOR "Light Green")
     (define COLOR-SPEC (if filled? GOO-COLOR "black"))]
    (if (> pipe-width tile-side-length)
        (square 1 "solid" "transparent")
        (if
         (and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
         (cond
           [(or (string=? dir "top") (string=? dir "bottom")) (overlay/align "middle" "top"
                                                                             (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                                                        COLOR-SPEC)
                                                                             (overlay/align "middle" "bottom"
                                                                                            (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                                                                       COLOR-SPEC)
                                                                                            (overlay/align "left" "middle"
                                                                                                           (rectangle (/ tile-side-length 2) pipe-width "solid" "black")
                                                                                                           (overlay/align "right" "middle"
                                                                                                                          (rectangle (/ tile-side-length 2) pipe-width "solid" "black")
                                                                                                                          (overlay (square pipe-width "solid" COLOR-SPEC)
                                                                                                                                   (square tile-side-length "solid" "Medium Gray"))))))]
             
           [(or (string=? dir "left") (string=? dir "right")) (overlay/align "right" "middle"
                                                                             (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                        COLOR-SPEC)
                                                                             (overlay/align "left" "middle"
                                                                                            (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                                       COLOR-SPEC)
                                                                                            (overlay/align "middle" "top"
                                                                                                           (rectangle pipe-width (/ tile-side-length 2) "solid" "black")
                                                                                                           (overlay/align "middle" "bottom"
                                                                                                                          (rectangle pipe-width (/ tile-side-length 2) "solid" "black")
                                                                                                                          (overlay (square pipe-width "solid" COLOR-SPEC)
                                                                                                                                   (square tile-side-length "solid" "Medium Gray"))))))])

         (overlay/align "middle" "top"
                        (rectangle pipe-width (/ tile-side-length 2) "solid"
                                   (if (pipe-top pipe) COLOR-SPEC "transparent"))
                        (overlay/align "middle" "bottom"
                                       (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                  (if (pipe-bot pipe) COLOR-SPEC "transparent"))
                                       (overlay/align "left" "middle"
                                                      (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                 (if (pipe-left pipe) COLOR-SPEC "transparent"))
                                                      (overlay/align "right" "middle"
                                                                     (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                (if (pipe-right pipe) COLOR-SPEC "transparent"))
                                                                     (overlay (square pipe-width "solid" COLOR-SPEC)
                                                                              (square tile-side-length "solid" "Medium Gray"))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(define GRID-4 (make-grid 9 LOP-1))


(define (grid-temp gd)
  (...
   (grid-dim gd)...
   (lop-temp (grid-lop gd))...))
  



; Create an example that represents an empty 7 × 7 grid called STARTING-GRID
(define STARTING-GRID (make-grid 7 '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (HW6 PART 2 TASK6)

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.

(check-expect (place-pipe STARTING-GRID PIPE-TL 3 3)
              (make-grid 7 (list (make-pipeNew (make-pipe #true #false #true #false #false) 3 3))))


(check-expect (place-pipe GRID-2 PIPE-TB 2 2)
              (make-grid 9 (list (make-pipeNew (make-pipe #true #true #false #false #false) 2 2)
                                 (make-pipeNew (make-pipe #true #false #true #false #false) 3 3))))

(check-expect (place-pipe GRID-3 PIPE-TR 1 1)
              (make-grid 9 (list (make-pipeNew (make-pipe #true #false #false #true #false) 1 1)
                                 (make-pipeNew (make-pipe #false #true #true #false #false) 5 5)
                                 (make-pipeNew (make-pipe #true #false #true #false #false) 3 3))))
              

  
              

(define (place-pipe grid pipe row col)
  (make-grid (grid-dim grid)
             (cons (make-pipeNew pipe col row) (grid-lop grid))))





;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.


(check-expect (pipe-at GRID-1 1 1) #false)
(check-expect (pipe-at GRID-2 3 3) (make-pipe #true #false #true #false #false))
(check-expect (pipe-at GRID-3 5 5) (make-pipe #false #true #true #false #false))

(define (pipe-at grid col row)
  (local
    [(define (is-pipe? ps)
       (and (= (pipeNew-x ps) col) (= (pipeNew-y ps) row)))
     (define my-filter (filter is-pipe? (grid-lop grid)))]
    (if (empty? my-filter)
        #false
        (pipeNew-pipes (first my-filter)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

; (HW6 PART 2 TASK7)

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
(define (lop->image lop tile-side-length pipe-width filled? imag)
  (cond
    [(empty? lop) imag]
    [(cons? lop) (place-image
                  (pipe->image (pipeNew-pipes (first lop)) tile-side-length pipe-width #false "top")
                  (+ (* (pipeNew-x (first lop)) tile-side-length) (/ tile-side-length 2))
                  (+ (* (pipeNew-y (first lop)) tile-side-length) (/ tile-side-length 2))
                  (lop->image (rest lop) tile-side-length pipe-width #false imag))]))


; lofp->image: [List-of Flow-pipe] Integer Integer Boolean Image --> Image
; draws the image of grid with list of Flow-pipe
(define (lofp->image lofp tile-side-length pipe-width filled? imag)
  (local
    [(define GOO-COLOR "Light Green")]
  (cond
    [(empty? lofp) imag]
    [(cons? lofp) (place-image (if (ormap (lambda (fp) (and (= (flow-pipe-col (first lofp)) (flow-pipe-col fp))
                                                            (= (flow-pipe-row (first lofp)) (flow-pipe-row fp)))) (rest lofp))
                                   (overlay/align "middle" "top"
                                                  (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                             (if filled? GOO-COLOR "black"))
                                                  (overlay/align "middle" "bottom"
                                                                 (rectangle pipe-width (/ tile-side-length 2) "solid"
                                                                            (if filled? GOO-COLOR "black"))
                                                                 (overlay/align "left" "middle"
                                                                                (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                           (if filled? GOO-COLOR "black"))
                                                                                (overlay/align "right" "middle"
                                                                                               (rectangle (/ tile-side-length 2) pipe-width "solid"
                                                                                                          (if filled? GOO-COLOR "black"))
                                                                                               (overlay (square pipe-width "solid" (if filled? GOO-COLOR "black"))
                                                                                                        (square tile-side-length "solid" "Medium Gray"))))))
                                   
                                   (pipe->image (flow-pipe-pipe (first lofp)) tile-side-length pipe-width filled? (flow-pipe-flow-dir (first lofp))))
                   (+ (* (flow-pipe-col (first lofp)) tile-side-length) (/ tile-side-length 2))
                   (+ (* (flow-pipe-row (first lofp)) tile-side-length) (/ tile-side-length 2))
                   (lofp->image (rest lofp) tile-side-length pipe-width filled? imag))])))
                   
                   



         

(define (grid->image grid tile-side-length pipe-width gf)
  (lofp->image (GooFlow-lofp gf) tile-side-length pipe-width #true
              (lop->image (grid-lop grid) tile-side-length pipe-width #false (create-blank-grid (grid-dim grid) tile-side-length))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (HW6 PART 3 TASK8)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (HW8  TASK3)

; Goo flows through the pipes on the grid, starting from the starting pipe.
; The starting pipe is considered to have goo from the beginning.
; The goo continues to propagate in the direction of the opening until it reaches either:

; An empty cell,
; The boundary of the grid, or
; A pipe that does not have an opening towards the direction of the flow.
; Design data called GooFlow that represents the path taken by the goo
; and the direction in which it is flowing. 

; Directions is one of:
; - "top"
; - "bottom"
; - "left"
; - "right"

;Interpretations: Represents the direction of the GooFlow will go

(define DIRECTION-LEFT "left")
(define DIRECTION-RIGHT "right")
(define DIRECTION-TOP "top")
(define DIRECTION-BOT "bottom")

(define (directions-temp dr)
  (...
   (cond
     [(string=? dr "top")...]
     [(string=? dr "bottom")...]
     [(string=? dr "left")...]
     [(string=? dr "right")...])))


(define-struct flow-pipe [pipe row col flow-dir])
; Flow-pipe is (make-flow-pipe Pipe Number Number Directions)

(define FP-1 (make-flow-pipe PIPE-TL 1 1 "top"))
(define FP-2 (make-flow-pipe PIPE-TR 2 2 "bottom"))
(define FP-3 (make-flow-pipe PIPE-TB 1 3 "right"))
(define FP-4 (make-flow-pipe PIPE-S1 1 3 "right"))

(define (flow-pipe-temp fp)
  (...
   (pipe-temp (flow-pipe-pipe fp))...
   (flow-pipe-row fp)...
   (flow-pipe-col fp)...
   (directions-temp (flow-pipe-flow-dir fp))...))

; A lofp is one of:
; -'()
; - (cons Flow-pipe [List-of Flow-pipe])
; List of Flow-pipes

(define LOFP-1 (list FP-1))
(define LOFP-2 (list FP-2))
(define LOFP-3 (list FP-1 FP-2 FP-3))
(define LOFP-4 (list FP-4))

(define (lofp-temp lofp)
  (...
   (cond
     [(empty? lofp)...]
     [(cons? lofp)
      (first lofp)...
      (lofp-temp (rest lofp))...])))



(define-struct GooFlow [lofp])
; GooFlow is (make-GooFlow [List-of flow-pipe])
; Represents the Goo Flowing in the game

(define GOOFLOW-1 (make-GooFlow LOFP-1))
(define GOOFLOW-2 (make-GooFlow LOFP-2))
(define GOOFLOW-3 (make-GooFlow LOFP-4))

(define (GooFlow-temp gf)
  (...
   (lofp-temp (GooFlow-lofp gf))...))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HW11 Task 2/Task 5: 

(define-struct gamestate [grid lops starting goof num-replaced time-counter])

; A GameState is (make-GameState Grid [List-of Pipe] PipeNew GooFlow Number Number)
; Interpretation: A GameState with a n*n background and a list of incoming pipes,
; the starting pipe with the location, the GooFlow that starts at the
; starting pipe location and finally the number of users clicked to replace the pipes
; and the time counter that counts the time

(define GAMESTATE-1 (make-gamestate GRID-1 LOPS-2 (make-pipeNew PIPE-S1 1 1) GOOFLOW-1 0 140))
(define GAMESTATE-2 (make-gamestate GRID-2 LOPS-2 (make-pipeNew PIPE-S2 2 2) GOOFLOW-1 2 130))
(define GAMESTATE-3 (make-gamestate GRID-3 FIXED-INCOMING-PIPES (make-pipeNew PIPE-S4 3 3) GOOFLOW-2 3 120))

                                    


(define (gamestate-temp ga)
  (... (grid-temp (gamestate-grid ga)) ...
       (lops-temp (gamestate-lops ga)) ...
       (pipeNew-temp (gamestate-starting ga))...
       (GooFlow-temp (gamestate-goof ga))...
       (gamestate-num-replaced ga)...))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HW8 TASK 4:
; Write a function grid-goo-propagate : GooFlow Grid -> GooFlow
; that moves the goo forward by one tile. If the goo is stuck, produce the same goo.

(define GRID-EXAMPLE (make-grid 4 (list (make-pipeNew PIPE-TR 2 1)
                                         (make-pipeNew PIPE-TB 2 0))))
(define GOOFLOW-EXAMPLE (make-GooFlow (list (make-flow-pipe PIPE-TB 0 2 DIRECTION-BOT))))

(check-expect (grid-goo-propagate GOOFLOW-EXAMPLE GRID-EXAMPLE) (make-GooFlow (list (make-flow-pipe PIPE-TR 1 2 DIRECTION-RIGHT)
                                                                                    (make-flow-pipe PIPE-TB 0 2 DIRECTION-BOT))))
   
 

(define GRID-EXAMPLE2 (make-grid 5 (list (make-pipeNew PIPE-TB 3 2)
                                         (make-pipeNew PIPE-TB 3 3))))

(define GOOFLOW-EXAMPLE2 (make-GooFlow (list (make-flow-pipe PIPE-TB 1 3 DIRECTION-BOT))))

(check-expect (grid-goo-propagate GOOFLOW-EXAMPLE2 GRID-EXAMPLE2) (make-GooFlow (list (make-flow-pipe PIPE-TB 2 3 DIRECTION-BOT)
                                                                                      (make-flow-pipe PIPE-TB 1 3 DIRECTION-BOT))))
                                                                                
(define GOOFLOW-EXAMPLE3 (make-GooFlow (list (make-flow-pipe PIPE-TB 2 3 DIRECTION-BOT)
                                             (make-flow-pipe PIPE-TB 1 3 DIRECTION-BOT))))
                                       

(check-expect (grid-goo-propagate GOOFLOW-EXAMPLE3 GRID-EXAMPLE2) (make-GooFlow (list (make-flow-pipe PIPE-TB 3 3 DIRECTION-BOT)
                                                                                      (make-flow-pipe PIPE-TB 2 3 DIRECTION-BOT)
                                                                                      (make-flow-pipe PIPE-TB 1 3 DIRECTION-BOT))))
                                                                                


(define (grid-goo-propagate gf grid)
  (local
    [(define LIST-OF-FLOW-PIPES (GooFlow-lofp gf))
     (define THE-DIRECTION (flow-pipe-flow-dir (first (GooFlow-lofp gf))))
     (define CURRENT-FP (first LIST-OF-FLOW-PIPES))

     ; calculate next pipe's x coordinate
     (define NEXT-X (if (string=? THE-DIRECTION DIRECTION-LEFT)
                        (- (flow-pipe-col CURRENT-FP) 1)
                        (if (string=? THE-DIRECTION DIRECTION-RIGHT)
                            (+ (flow-pipe-col CURRENT-FP) 1)
                            (flow-pipe-col CURRENT-FP))))
     ; calculate next pipe's y coordinate
     (define NEXT-Y (if (string=? THE-DIRECTION DIRECTION-TOP)
                        (- (flow-pipe-row CURRENT-FP) 1)
                        (if (string=? THE-DIRECTION DIRECTION-BOT)
                            (+ (flow-pipe-row CURRENT-FP) 1)
                            (flow-pipe-row CURRENT-FP))))    
     ; the next pipe it will go to using the pipe-at function defined in hw6
     (define NEXT-PIPE (pipe-at grid NEXT-X NEXT-Y))]
    (cond
      [(false? NEXT-PIPE) gf] 
      [(or (< NEXT-X 0) (< NEXT-Y 0) (>= NEXT-X (grid-dim grid)) (>= NEXT-Y (grid-dim grid))) gf] 
      [(and (string=? THE-DIRECTION DIRECTION-LEFT) (not (pipe-right NEXT-PIPE))) gf]
      [(and (string=? THE-DIRECTION DIRECTION-RIGHT) (not (pipe-left NEXT-PIPE))) gf]
      [(and (string=? THE-DIRECTION DIRECTION-TOP) (not (pipe-bot NEXT-PIPE))) gf]
      [(and (string=? THE-DIRECTION DIRECTION-BOT) (not (pipe-top NEXT-PIPE))) gf] ;these are all situations need to return original gf
      [else
       (make-GooFlow (cons (make-flow-pipe NEXT-PIPE NEXT-Y NEXT-X (cond
                                                                     [(and (pipe-top NEXT-PIPE) (pipe-bot NEXT-PIPE) (pipe-left NEXT-PIPE) (pipe-right NEXT-PIPE)) THE-DIRECTION]
                                                                     [(and (pipe-top NEXT-PIPE) (not (string=? DIRECTION-BOT THE-DIRECTION))) DIRECTION-TOP]
                                                                     [(and (pipe-bot NEXT-PIPE) (not (string=? DIRECTION-TOP THE-DIRECTION))) DIRECTION-BOT]
                                                                     [(and (pipe-left NEXT-PIPE) (not (string=? DIRECTION-RIGHT THE-DIRECTION))) DIRECTION-LEFT]
                                                                     [(and (pipe-right NEXT-PIPE) (not (string=? DIRECTION-LEFT THE-DIRECTION))) DIRECTION-RIGHT])) LIST-OF-FLOW-PIPES))])))
                     



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HW8 Task 6:
; Design a function called gamestate-init that initializes the gamestate given the following information:

; the grid dimension
; the x and y grid coordinates of the starting pipe
; the direction of the starting pipe
; the incoming pipes list

; gamestate-init: Number Number Number Directions [List-of Pipe] -> GameState
; purpose: initializes the gamestate given the above information


(define INCOMING-PIPES (list PIPE-TL PIPE-TR PIPE-LR PIPE-LR))
(define INCOMING-PIPES/V2 (list PIPE-TL PIPE-LR PIPE-TB-LR PIPE-TB))
(define INCOMING-PIPES/V3 (append INCOMING-PIPES INCOMING-PIPES/V2))

(check-expect (gamestate-init 7 2 2 "left" INCOMING-PIPES) (make-gamestate
                                                            (make-grid 7 (list (make-pipeNew (make-pipe #false #false #true #false #true) 2 2)))
                                                            (list
                                                             (make-pipe #true #false #true #false #false)
                                                             (make-pipe #true #false #false #true #false)
                                                             (make-pipe #false #false #true #true #false)
                                                             (make-pipe #false #false #true #true #false))
                                                            (make-pipeNew (make-pipe #false #false #true #false #true) 2 2)
                                                            (make-GooFlow (list (make-flow-pipe (make-pipe #false #false #true #false #true) 2 2 "left"))) 0 140))

(check-expect (gamestate-init 6 2 2 "right" INCOMING-PIPES) (make-gamestate
                                                             (make-grid 6 (list (make-pipeNew (make-pipe #false #false #false #true #true) 2 2)))
                                                             (list
                                                              (make-pipe #true #false #true #false #false)
                                                              (make-pipe #true #false #false #true #false)
                                                              (make-pipe #false #false #true #true #false)
                                                              (make-pipe #false #false #true #true #false))
                                                             (make-pipeNew (make-pipe #false #false #false #true #true) 2 2)
                                                             (make-GooFlow (list (make-flow-pipe (make-pipe #false #false #false #true #true) 2 2 "right"))) 0 140))


(check-expect (gamestate-init 5 3 3 "top" INCOMING-PIPES) (make-gamestate
                                                           (make-grid 5 (list (make-pipeNew (make-pipe #true #false #false #false #true) 3 3)))
                                                           (list
                                                            (make-pipe #true #false #true #false #false)
                                                            (make-pipe #true #false #false #true #false)
                                                            (make-pipe #false #false #true #true #false)
                                                            (make-pipe #false #false #true #true #false))
                                                           (make-pipeNew (make-pipe #true #false #false #false #true) 3 3)
                                                           (make-GooFlow (list (make-flow-pipe (make-pipe #true #false #false #false #true) 3 3 "top"))) 0 140))

(check-expect (gamestate-init 5 1 1 "bottom" INCOMING-PIPES) (make-gamestate
                                                              (make-grid 5 (list (make-pipeNew (make-pipe #false #true #false #false #true) 1 1)))
                                                              (list
                                                               (make-pipe #true #false #true #false #false)
                                                               (make-pipe #true #false #false #true #false)
                                                               (make-pipe #false #false #true #true #false)
                                                               (make-pipe #false #false #true #true #false))
                                                              (make-pipeNew (make-pipe #false #true #false #false #true) 1 1)
                                                              (make-GooFlow (list (make-flow-pipe (make-pipe #false #true #false #false #true) 1 1 "bottom"))) 0 140))


(define (gamestate-init dimm x y dir lops)
  (local [(define PIPE-FOR-START 
           (cond
             [(string=? dir "right") (make-pipe #false #false #false #true #true)]
             [(string=? dir "bottom") (make-pipe #false #true #false #false #true)]
             [(string=? dir "top") (make-pipe #true #false #false #false #true)]
             [(string=? dir "left") (make-pipe #false #false #true #false #true)]))]

    (make-gamestate (make-grid dimm (list (make-pipeNew PIPE-FOR-START x y))) lops
                    (make-pipeNew PIPE-FOR-START x y)
                    (make-GooFlow (list (make-flow-pipe PIPE-FOR-START y x dir))) 0 140)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HW8 Task 7:
; Modify your program to display up to n incoming pipes.
; ($n$ should be greater than 3, but can be any number you like.)

; Hint: We recommend adding this to the right or bottom of grid,
; so the grid stays at same mouse coordinate and your computation for mouse coordinates
; to grid coordinates are not affected. You might want to add a check in place-pipe-on-click
; to make sure nothing happens if mouse is clicked outside the grid.

; Test your code: all existing check-expects should work.
; The game should also work and show the incoming pipes. Goo doesn’t yet flow, but that’s ok.



; beside-incoming-pipes-with-main: Grid [List-of Pipe] Number Number -> Image
; Interpretation: The function places the image of the incoming pipe list beside the
; image of the original grid

(define (beside-incoming-pipes-with-main grid lops tile-side-length pipe-width gf)
  (local
    [; This is to draw my incoming pipes withou grid
     (define (place-incoming-pipes lops tile-side-length pipe-width acc)
       (cond
         [(or (> acc 4) (empty? lops)) (rectangle 1 1 "solid" "transparent")]
         [(cons? lops)
          (above (pipe->image (first lops) tile-side-length pipe-width #false "top")
                 (place-incoming-pipes (rest lops) tile-side-length pipe-width (+ 1 acc)))]))]
  (beside (grid->image grid tile-side-length pipe-width gf)
          (rectangle 40 tile-side-length "solid" "transparent")
          (place-incoming-pipes lops tile-side-length pipe-width 0))))
          



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (HW11 TASK 4)

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. if the user tries to place an incoming pipe
; on a pipe that already has goo in it, then the pipe should not be placed.


(check-expect (place-pipe-on-click GAMESTATE-1 600 200 "button-up")
              (make-gamestate (make-grid 7 (list (make-pipeNew (make-pipe #true #false #true #false #false) 6 2))) '() (make-pipeNew PIPE-S1 1 1) GOOFLOW-1 0 139))

(check-expect (place-pipe-on-click GAMESTATE-2 800 400 "button-up")
              (make-gamestate (make-grid 9 (list (make-pipeNew (make-pipe #true #false #true #false #false) 8 4)
                                                 (make-pipeNew (make-pipe #true #false #true #false #false) 3 3))) '() (make-pipeNew PIPE-S2 2 2) GOOFLOW-1 2 129))

(check-expect (place-pipe-on-click GAMESTATE-3 750 500 "button-up")
              (make-gamestate (make-grid 9 (list
                                            (make-pipeNew (make-pipe #true #false #true #false #false) 7 5)
                                            (make-pipeNew (make-pipe #false #true #true #false #false) 5 5)
                                            (make-pipeNew (make-pipe #true #false #true #false #false) 3 3)))
                              (list
                               (make-pipe #false #false #true #true #false)
                               (make-pipe #true #true #false #false #false)
                               (make-pipe #true #true #true #true #false)
                               (make-pipe #false #true #false #true #false)) (make-pipeNew PIPE-S4 3 3) GOOFLOW-2 3 119))


(define (place-pipe-on-click initial-game-state x y me)
  (if
   (and (mouse=? me "button-up") (<= x (* TSL (grid-dim (gamestate-grid initial-game-state))))
        (<= y (* TSL (grid-dim (gamestate-grid initial-game-state)))))
   (if
    (empty? (gamestate-lops initial-game-state))
    (make-gamestate (gamestate-grid initial-game-state) (gamestate-lops initial-game-state)
                    (gamestate-starting initial-game-state) (grid-goo-propagate (gamestate-goof initial-game-state) (gamestate-grid initial-game-state)) (gamestate-num-replaced initial-game-state) (gamestate-time-counter initial-game-state))
    (if
     (ormap (lambda (fp) (and (= x (flow-pipe-col fp)) (= y (flow-pipe-row fp)))) (GooFlow-lofp (gamestate-goof initial-game-state)))
     initial-game-state
     (make-gamestate
      (place-pipe (gamestate-grid initial-game-state) (first (gamestate-lops initial-game-state)) (floor (/ y TSL)) (floor (/ x TSL)))
      (rest (gamestate-lops initial-game-state)) (gamestate-starting initial-game-state) (gamestate-goof initial-game-state) (if (not (pipe? (pipe-at (gamestate-grid initial-game-state) x y)))
                                                                                                                                 (gamestate-num-replaced initial-game-state)
                                                                                                                                 (+ 1 (gamestate-num-replaced initial-game-state))) (- (gamestate-time-counter initial-game-state) 1))))
    initial-game-state))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (HW11 TASK2)

;get-score: GameState -> Integer
; computes the current score of the game.

(define (get-score gs)
  (* 50 (- (length (GooFlow-lofp (gamestate-goof gs))) (gamestate-num-replaced gs) 1)))
  

(check-expect (get-score GAMESTATE-1) 0)
(check-expect (get-score GAMESTATE-2) -100)
(check-expect (get-score GAMESTATE-3) -150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (HW11 TASK3)


; draw-image: Gamestate -> Image
; Display the current score while the game is running
; on the grid with incoming pipe images also beside
(define (draw-image initial-game-state)
  (beside 
   (beside-incoming-pipes-with-main
    (gamestate-grid initial-game-state)
    (gamestate-lops initial-game-state)
    TSL
    PIPE-WIDTH
    (gamestate-goof initial-game-state))
   (rectangle 40 30 "solid" "transparent")
   (text (number->string (get-score initial-game-state)) 30 "black")
   (rectangle TSL PIPE-WIDTH  "solid" "transparent")))


;; pipe-fantasy: GameState -> GameState
;
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    (to-draw draw-image)
    (on-tick time-tick)
    (on-mouse place-pipe-on-click)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HW11 Task 6:

; Time-tick: GameState -> Gamestate
; If there is time left before the next goo propagation, then count down the time by 1 tick.
; If the countdown timer reaches 0, then propagate the goo by one cell and
; reset the countdown timer to 28 ticks (1 second).


;Require three check-expects
(check-expect (time-tick GAMESTATE-1) (make-gamestate (make-grid 7 '()) (list PIPE-TL)
                                       (make-pipeNew (make-pipe #true #false #false #false #true) 1 1)
                                       (make-GooFlow (list (make-flow-pipe PIPE-TL 1 1 "top"))) 0 139))


(check-expect (time-tick GAMESTATE-2) (make-gamestate (make-grid 9 (list (make-pipeNew PIPE-TL 3 3))) (list PIPE-TL)
                                                      (make-pipeNew (make-pipe #false #true #false #false #true) 2 2)
                                                      (make-GooFlow (list (make-flow-pipe PIPE-TL 1 1 "top"))) 2 129))

(check-expect (time-tick GAMESTATE-3) (make-gamestate (make-grid 9 (list (make-pipeNew PIPE-BL 5 5)
                                                                         (make-pipeNew PIPE-TL 3 3)))
                                                      (list PIPE-TL PIPE-LR PIPE-TB PIPE-TB-LR PIPE-BR)
                                                      (make-pipeNew (make-pipe #false #false #false #true #true) 3 3)
                                                      (make-GooFlow (list (make-flow-pipe PIPE-TR 2 2 "bottom")))3 119))

(define (time-tick gs)
  (cond
    [(> (gamestate-time-counter gs) 0)
     (make-gamestate (gamestate-grid gs) (gamestate-lops gs) (gamestate-starting gs)
                     (gamestate-goof gs) (gamestate-num-replaced gs) (- (gamestate-time-counter gs) 1))]
    [(= (gamestate-time-counter gs) 0)
     (make-gamestate (gamestate-grid gs) (gamestate-lops gs) (gamestate-starting gs)
                     (grid-goo-propagate (gamestate-goof gs) (gamestate-grid gs)) (gamestate-num-replaced gs)
                     140)]))


        
