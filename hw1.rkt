;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Purpose: An introduction to data design (enumerations) and the design recipe.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 3. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).


;;! Problem 1

;; Design a function called concat-space-separator-when-long that consumes two
;; strings and produces a single string that concatenates them. In the result,
;; the two strings should be separated by a space *only if* the first string
;; is longer than 5 characters.


; concat-space-separator-when-long: String String - > String
; Concatenate the two strings and if the first string is longer than 5 characters,
; seperate them by a space

(check-expect (concat-space-separator-when-long "lllll" "kkk") "lllllkkk")
(check-expect (concat-space-separator-when-long "i" "am") "iam")
(check-expect (concat-space-separator-when-long "themselves" "have") "themselves have")


(define (concat-space-separator-when-long string1 string2)
  (if
    (> (string-length string1) 5) (string-append string1 " " string2) (string-append string1 string2)))



;;! Problem 2

;;! Part A

;; Our solar systems traditionally had nine planets. Look them up, and
;; write a data definition called Planet that can represent any one of them.
;; NOTE: name your template planet-template.

; planet is one of: 
; - "Mercury"
; - "Venus"
; - "Earth"
; - "Mars"
; - "Jupiter"
; - "Saturn"
; - "Uranus"
; - "Neptune"
; - "Pluto"

;Interpretation: planets of solar system

(define PT-MER "Mercury")
(define PT-VEN "Venus")
(define PT-EAR "Earth")
(define PT-MAR "Mars")
(define PT-JUP "Jupiter")
(define PT-SAT "Saturn")
(define PT-URA "Uranus")
(define PT-NEP "Neptune")
(define PT-PLU "Pluto")

(define (planet-template pt)
  (...(cond
    [(string=? pt PT-MER) ...]
    [(string=? pt PT-VEN) ...]
    [(string=? pt PT-EAR) ...]
    [(string=? pt PT-MAR) ...]
    [(string=? pt PT-JUP) ...]
    [(string=? pt PT-SAT) ...]
    [(string=? pt PT-URA) ...]
    [(string=? pt PT-NEP) ...]
    [(string=? pt PT-PLU) ...])))
    
  





;;! Part B

;; One way to classify planets is as either terrestrial, gas giant, or dwarf planet.
;; Design a function called planet-kind that consumes a Planet and produces either
;; "terrestrial", "gas giant", or "dwarf planet".

; planet-kind:Planet -> String
; the function consumes a planet and decide which category of planet to put it in
; "terrestrial", "gas giant", or "dwarf planet"

(check-expect (planet-kind PT-MER) "terrestrial")
(check-expect (planet-kind PT-VEN) "terrestrial")
(check-expect (planet-kind PT-EAR) "terrestrial")
(check-expect (planet-kind PT-MAR) "terrestrial")
(check-expect (planet-kind PT-JUP) "gas giant")
(check-expect (planet-kind PT-URA) "gas giant")
(check-expect (planet-kind PT-SAT) "gas giant")
(check-expect (planet-kind PT-NEP) "gas giant")
(check-expect (planet-kind PT-PLU) "dwarf planet")

(define (planet-kind pt)
  (cond
    [(or (or (or (string=? pt PT-MER) (string=? pt PT-VEN)) (string=? pt PT-EAR)) (string=? pt PT-MAR))
     "terrestrial"]
    [(or (or (or (string=? pt PT-JUP) (string=? pt PT-URA)) (string=? pt PT-SAT)) (string=? pt PT-NEP))
     "gas giant"]
    [else
     "dwarf planet"]))


(planet-kind PT-PLU)
    
  
  


;;! Part C

;; Design a predicate called has-moons? that produces true if a planet has any
;; moons.

;has-moons?: Planet -> Boolean
;Purpose: To check whether the planet given to the function has any moons


(check-expect (has-moons? PT-MER) false)
(check-expect (has-moons? PT-VEN) false)
(check-expect (has-moons? PT-EAR) true)
(check-expect (has-moons? PT-MAR) true)
(check-expect (has-moons? PT-JUP) true)
(check-expect (has-moons? PT-URA) true)
(check-expect (has-moons? PT-SAT) true)
(check-expect (has-moons? PT-NEP) true)
(check-expect (has-moons? PT-PLU) true)


(define (has-moons? pt)
  (cond
    [(string=? pt PT-MER) #false]
    [(string=? pt PT-VEN) #false]
    [(string=? pt PT-EAR) #true]
    [(string=? pt PT-MAR) #true]
    [(string=? pt PT-JUP) #true]
    [(string=? pt PT-URA) #true]
    [(string=? pt PT-SAT) #true]
    [(string=? pt PT-NEP) #true]
    [(string=? pt PT-PLU) #true]))


    
 

;;! Problem 3

;;! Part A

;; Design a data definition called RainbowColor that represents a color of the
;; rainbow. To avoid ambiguity, use the "modern" colors from this Wikipedia page:
;; https://en.wikipedia.org/wiki/Rainbow
;; NOTE: call your template rainbow-color-template.

; RainbowColor is one of:
; -"Red"
; -"Orange"
; -"Yellow"
; -"Green"
; -"Cyan"
; -"Blue"
; -"Violet"

;Interpretation: The colors from the most outside of
; the rainbow to the innermost color of the rainbow

(define R "Red")
(define O "Orange")
(define Y "Yellow")
(define G "Green")
(define C "Cyan")
(define B "Blue")
(define V "Violet")

(define (rainbow-color-template color)
  (...(cond
    [(string=? color R) ...]
    [(string=? color O) ...]
    [(string=? color Y) ...]
    [(string=? color G) ...]
    [(string=? color C) ...]
    [(string=? color B) ...]
    [(string=? color V) ...])))
  
 

;;! Part B

;; Design a predicate called primary? to determine if a RainbowColor is a primary
;; color (red, yellow, or blue).

; primary?: RainbowColor -> Boolean
; Purpose: To check whether the RainbowColor given is the primary color


(check-expect (primary? R) true)
(check-expect (primary? O) false)
(check-expect (primary? Y) true)
(check-expect (primary? G) false)
(check-expect (primary? C) false)
(check-expect (primary? B) true)
(check-expect (primary? V) false)


(define (primary? color)
  (cond
    [(string=? color R) true]
    [(string=? color O) false]
    [(string=? color Y) true]
    [(string=? color G) false]
    [(string=? color C) false]
    [(string=? color B) true]
    [(string=? color V) false]))
                        

;;! Part C

;; Design a function called next-color that consumes a RainbowColor and produces
;; the next color, where next goes from outside to inside of a rainbow. When
;; applies to the innermost color (violet), it produces the outermost color (red).

; next-color: RainbowColor -> String
; Purpose: According the input rainbow color, output the next color

(check-expect (next-color R) "orange")
(check-expect (next-color O) "yellow")
(check-expect (next-color Y) "green")
(check-expect (next-color G) "cyan")
(check-expect (next-color C) "blue")
(check-expect (next-color B) "violet")
(check-expect (next-color V) "red")



(define (next-color color)
  (cond
    [(string=? color R) "orange"]
    [(string=? color O) "yellow"]
    [(string=? color Y) "green"]
    [(string=? color G) "cyan"]
    [(string=? color C) "blue"]
    [(string=? color B) "violet"]
    [(string=? color V) "red"]))


    















