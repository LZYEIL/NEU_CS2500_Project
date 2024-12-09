;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))


;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (= (first lon) 0)
         '()
         (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
; Interpretation: A list of elements of type X

(define LOS-1 '())
(define LOS-2 (cons "klu" LOS-1))
(define LOS-3 (cons 80 (cons "opl" LOS-2)))


(define (listOfx-temp lox)
  (...
   (cond
     [(empty? lox)...]
     [(cons? lox)
      (first lox)...
      (listOfx-temp (rest lox))...])))
     

;helper for the second function:
; until-zero-helper: Number -> Boolean
; To check if the number input equals to 0

(check-expect (until-zero-helper 5) #false)
(check-expect (until-zero-helper 4) #false)
(check-expect (until-zero-helper 0) #true)

(define (until-zero-helper n)
  (= n 0))





;helper for the third function:
; words-until-period: String-> Boolean
; To check if the string input equals to the period

(check-expect (words-until-period-helper "j") #false)
(check-expect (words-until-period-helper ".") #true)
(check-expect (words-until-period-helper "k") #false)

(define (words-until-period-helper s)
  (string=? s "."))



; until-something: (X -> Boolean) [List-of X] -> [List-of X]
; Purpose: To modify the list of x based on some conditions

(check-expect (until-something words-until-period-helper (list "k" "." "m"))
              (list "k"))

(check-expect (until-something words-until-period-helper (list "m" "." "m"))
              (list "m"))

(check-expect (until-something until-zero-helper (list 50 0 60))
              (list 50))




(define (until-something func lox)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (if (func (first lox)) 
         '()
         (cons (first lox) (until-something func (rest lox))))]))
         


;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or words-until-period/v2.


; until-zero/v2: [List-of X] -> [List-of X]
; output the list of numbers until zero

(define (until-zero/v2 lox)
  (until-something until-zero-helper lox))




; words-until-period/v2: [List-of X] -> [List-of X]
; output the list of strings until the period

(define (words-until-period/v2 lox)
  (until-something words-until-period-helper lox))



;;! Problem 2

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.

(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of any type
;; - first is the first item in the pair
;; - second is the second item in the pair

(define (pair-temp pr)
  (...
   (pair-first pr)...
   (pair-second pr)...))


;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())

;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair.
(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "is")))
(check-expect (alternate-case (list (make-pair "one" "two") (make-pair "three" "four") (make-pair "five" "six")))
              (list (make-pair "ONE" "two") (make-pair "THREE" "four") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana"))) (list (make-pair "APPLE" "banana")))

;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second.
(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))

;; However, you must not _directly_ use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.



; A [List-of [Pair X]] (LoP) is one of:
; - '()
; - (cons [Pair X] LoP)

;Interpretation: A list of [Pair X] elements

(define LOP-1 '())
(define LOP-2 (cons (make-pair "hello" "world") LOP-1))
(define LOP-3 (cons (make-pair 20 40) LOP-2))


(define (lop-temp lop)
  (...
   (cond
     [(empty? lop)...]
     [(cons? lop)
      (pair-temp (first lop))...
      (lop-temp (pair-temp (rest lop)))...])))





; strings-or-odds-helper: Number -> String
; converts a number to the string "odd"

(check-expect (strings-or-odds-helper 20) "odd")
(check-expect (strings-or-odds-helper 0) "odd")
(check-expect (strings-or-odds-helper -50) "odd")

(define (strings-or-odds-helper num)
  "odd")



; Now abstracting the helper of every function above!!!

; operateElements-abs: (X Y) [Pair X] (X -> Y) (X -> X) -> [Pair Y]
; output a pair where the first and second fields of it are modified


(check-expect (operateElements-abs (make-pair 30 40) number->string strings-or-odds-helper)
              (make-pair "30" "odd"))

(check-expect (operateElements-abs (make-pair "j" "K") string-upcase identity)
              (make-pair "J" "K"))

(check-expect (operateElements-abs (make-pair #true #false) not identity)
              (make-pair #false #false))

              
(define (operateElements-abs pr func1 func2)
  (make-pair (func1 (pair-first pr)) (func2 (pair-second pr))))






; This is the abstraction of the main function:!!!!
; do-to-all: (X Y) [List-of [Pair X]] (X -> Y) (X -> X) -> [List-of [Pair Y]]
; abstracts the main function above to modify both the first and the second
; elements in th (LoP)

(check-expect (do-to-all (list (make-pair 20 30) (make-pair 50 60)) number->string strings-or-odds-helper)
              (list (make-pair "20" "odd") (make-pair "50" "odd")))

(check-expect (do-to-all (list (make-pair "k" "m") (make-pair "j" "t")) string-upcase identity)
              (list (make-pair "K" "m") (make-pair "J" "t")))

(check-expect (do-to-all (list (make-pair #true #false) (make-pair #true #false)) not identity)
              (list (make-pair #false #false) (make-pair #false #false)))


              
(define (do-to-all lop func1 func2)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cons (operateElements-abs (first lop) func1 func2)
           (do-to-all (rest lop) func1 func2))]))




(define (strings-or-odds lop)
  (do-to-all lop number->string strings-or-odds-helper))



(define (alternate-case lop)
  (do-to-all lop string-upcase identity))



(define (flip-or-keep-boolean lop)
   (do-to-all lop not identity))


;;! Problem 3

;; Objective: Build a Word Game

;; Your goal is to author a word-building game. You will start with an empty 5x1 grid
;; and a hidden list of random letters. When the player clicks on a cell, its
;; contents should be replaced by the next letter in the list. The game concludes
;; when the cells spell a five-letter word. (You should build a short list of
;; five letter words.)
;;
;; Here is a video that demonstrates the game:
;;
;;   https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw5.gif
;;
;; Here are questions to help you think through your program design:
;;
;; 1. What do you need in your world state? (What changes during the game?)
;;    Come up with a data design to represent the world state.
;;
;; 2. Your program needs to draw a board, handle mouse clicks, and stop when
;;    the player constructs a word or runs out of letters. These are three 
;;    functions that you need to design.
;;
;; 3. Finally, put it all together using big-bang.


(require 2htdp/image)
(require 2htdp/universe)

; THESE ARE CONSTANTS:!!!!

(define BOX-WIDTH 50)
(define BOX (square BOX-WIDTH "outline" "black"))
(define BOX-OUTLINE-WIDTH 5)
(define FONT-SIZE 16)

(define LETTERLIST (list "a" "c" "d" "b" "k" "o" "l" "n" "e" "f" "g" "h" "i" "p" "q" "r" "s" ))
(define WORDS (list "bacon" "clank" "alone" "acorn" "banco" "cloak" "knead" "blond" "colon" "clean"))



; DATA DEFINITION:!!!

; A [List-of String] is one of:
; - '()
; - (cons String [List-of String])
; Interpretation: A list of String elements

(define LOSTRING-1 '())
(define LOSTRING-2 (cons "zkl" LOS-1))
(define LOSTRING-3 (cons "pol" (cons "opl" LOS-2)))


(define (listOfs-temp los)
  (...
   (cond
     [(empty? los)...]
     [(cons? los)
      (first los)...
      (listOfs-temp (rest los))...])))





; STRUCT DEFINITION:!!!!

(define-struct states [chars alphabets])
; states is (make-states String [List-of String])
; - chars is the letter of the letterlist above
; - alphabets is the word from the words list above

(define STATE1 (make-states "k" WORDS))

(define (states-temp stat)
  (...
   (listOfs-temp (states-chars stat)...)
   (listOfs-temp (states-alphabets stat)...)))





;HELPER FUNCTION NEEDED (MOUSE-ACTION):

; update-chars: [List-of String] Number [List-of String] -> [List-of String]
; replace a single character in a string with a given, new character


(check-expect (update-chars "k" 0 "l") "l")
(check-expect (update-chars "kljmkshf" 3 "l") "kljlkshf")
(check-expect (update-chars "wdqwdbobdo" 4 "z") "wdqwzbobdo")


(define (update-chars crs index new)
  (string-append
   (substring crs 0 index)
   new
   (substring crs (+ index 1))))



; function of mouse-action!!!

; mouse-action: States Number Number MouseEvent -> States
; handles a mouse event, updating the corresponding grid
; when the mouse clicks

(check-expect (mouse-action STATE1 20 30 "button-down") (make-states "bacon" (list "clank"
                                                                                   "alone"
                                                                                   "acorn"
                                                                                   "banco"
                                                                                   "cloak"
                                                                                   "knead"
                                                                                   "blond"
                                                                                   "colon"
                                                                                   "clean")))

(check-expect (mouse-action (make-states "klkgmkhjh" WORDS) 60 90 "button-down") (make-states "kbaconkgmkhjh"
                                                                                              (list
                                                                                               "clank"
                                                                                               "alone"
                                                                                               "acorn"
                                                                                               "banco"
                                                                                               "cloak"
                                                                                               "knead"
                                                                                               "blond"
                                                                                               "colon"
                                                                                               "clean")))

(check-expect (mouse-action (make-states "hjhjgkuti" WORDS) 160 50 "button-down") (make-states "hjhbacongkuti"
                                                                                               (list
                                                                                                "clank"
                                                                                                "alone"
                                                                                                "acorn"
                                                                                                "banco"
                                                                                                "cloak"
                                                                                                "knead"
                                                                                                "blond"
                                                                                                "colon"
                                                                                                "clean")))


(check-expect (mouse-action (make-states "hjhjgkuti" WORDS) 140 50 "button-down") (make-states "hjbaconjgkuti"
                                                                                               (list
                                                                                                "clank"
                                                                                                "alone"
                                                                                                "acorn"
                                                                                                "banco"
                                                                                                "cloak"
                                                                                                "knead"
                                                                                                "blond"
                                                                                                "colon"
                                                                                                "clean")))

(check-expect (mouse-action (make-states "hjhjgkuti" WORDS) 480 50 "button-down") (make-states "hjhjgkuti"
                                                                                               (list
                                                                                                "bacon"
                                                                                                "clank"
                                                                                                "alone"
                                                                                                "acorn"
                                                                                                "banco"
                                                                                                "cloak"
                                                                                                "knead"
                                                                                                "blond"
                                                                                                "colon"
                                                                                                "clean")))





(define (mouse-action st x y Mevent)
  (if
   (and (string=? Mevent "button-down")
        (and (< x (* 5 BOX-WIDTH)) (> x 0)))
   (cond
     [(< x BOX-WIDTH) (make-states (update-chars (states-chars st)
                                                 0
                                                 (first (states-alphabets st)))
                                   (rest (states-alphabets st)))]
     [(< x (* 2 BOX-WIDTH)) (make-states (update-chars (states-chars st)
                                                 1
                                                 (first (states-alphabets st)))
                                   (rest (states-alphabets st)))]
     [(< x (* 3 BOX-WIDTH)) (make-states (update-chars (states-chars st)
                                                 2
                                                 (first (states-alphabets st)))
                                   (rest (states-alphabets st)))]
     [(< x (* 4 BOX-WIDTH)) (make-states (update-chars (states-chars st)
                                                 3
                                                 (first (states-alphabets st)))
                                   (rest (states-alphabets st)))]
     [else (make-states (update-chars (states-chars st)
                                                 4
                                                 (first (states-alphabets st)))
                                   (rest (states-alphabets st)))])
   st))
     
                                                 
   

; draw-chars: String -> Image
; draw a string of chars as adjacent boxes


(check-expect (draw-chars "") (square 1 "solid" "transparent"))
#;(check-expect (draw-chars "hjf") (beside (overlay/align "middle" "center" (text (substring "hjf" 0 1) FONT-SIZE "black") BOX)
                                         (square 1 "solid" "transparent")))


              



(define (draw-chars crs) 
  (cond
    [(string=? crs "") (square 1 "solid" "transparent")]
    [else
     (beside (overlay/align "middle" "center" (text (substring crs 0 1) FONT-SIZE "black") BOX)
             (draw-chars (substring crs 1)))]))



; this can be thrown into the big bang function:
; draw-states: States -> Image
;draw the current state of the game
(define (draw-states st)
  (draw-chars (states-chars st)))



; word?: String [List-of String] -> Boolean
; check if the given word is present in a list of words


(define (word? chars words)
  (cond
    [(empty? words) #false]
    [(cons? words) (or (string=? (first words) chars)
                      (word? chars (rest words)))]))


; stop?: States -> Boolean
; checks whether if the game has ended, returning true if ended, and false otherwise


;(check-expect (stop? (make-states 
(define (stop? st)
  (or (empty? (states-alphabets st))
      (word? (states-chars st) WORDS)))




; BIGBANG
(define (word-building initial-state)
  (big-bang (make-states "     " LETTERLIST)
    (to-draw draw-states)
    (on-mouse mouse-action)
    (stop-when stop? draw-states)))

   



















