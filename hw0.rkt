;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;;! Purpose: An introduction to programming with simple function definitions.

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.


(require 2htdp/image)

;;! Problem 1

;;! Part A

;; Examine the following function, and:
;; i.   Write down its signature,
;; ii.  Give it an informative purpose statement, and
;; iii. Give the function and its arguments more informative names.

;; [TODO] Signature
;detective: number number string -> string

;; [TODO] Purpose
;Test whether the passenger is qulified to board


(define (detective a b c)
  (if (< (+ (* 12 a) b) 48)
      (string-append "Welcome aboard, " c "!")
      "Sorry, you may not board the ride. :("))

;; [TODO] Better names for detective and its arguments
; detective: ABLE-TO-BOARD
; a: AGE
; b: WEIGHT
; c: PASSENGER_NAME



;;! Part B

;; Write the signature of the following function:

;; [TODO] Signature
; mystery: string string number -> string



(define (mystery x y z)
  (string-append (substring x 0 z)
                 (substring y z)
                 (substring y 0 z)
                 (substring x z)))


;;! Part C

;; Describe the values that mystery produces when you apply it to two identical
;; arguments for x and y.

;; [TODO] Prose description as a comment.

; When the x and y are two identical arguments,
;  the mystery produces the single argument 2 times no matter the value of z is.


;;! Problem 2

;; You and your friend have been arguing about how best to invest some money.
;; You think you've picked some stocks that give decent gains consistently, but
;; your friend really wants to invest in cryptocurrencies, which she believes
;; have substantially larger gains some good days, but also suffer some losses
;; on some bad days. To settle the debate of which to invest in, you offer to
;; program a simulation of the two choices.

;;! Part A

;; Define a function stock-day which simulates a single day of gain from the
;; stocks you have in mind. Specifically, stock-day should receive as its
;; argument the amount of money you have, and should produce the amount you
;; will have after a 4% gain. For example, (stock-day 1000) should produce 1040.
;; In addition, write three examples. Here is one to get you started:
;;
;; (stock-day 1000) ; produces 1040

;; [TODO] Function definition

(define (STOCK-DAY SD)
  (* SD 1.04))

;; [TODO] Examples
;(stock-day 10000) gives 10400
;(stock-day 20000) gives 20800
;(stock-day 30000) gives 31200







;;! Part B

;; Define a function crypto-good-day which simulates a single day of gain from
;; cryptocurrencies, assuming it was a good day. Specifically, crypto-good-day
;; should calculate the amount of money you will have after a 10% gain.
;; You must also write three examples of this function.

;; [TODO] Function definition

(define (CRYPTO-GOOD-DAY CD)
  (* CD 1.1))
  



;; [TODO] Three examples
;(CRYPTO-GOOD-DAY 10000) gives 11000
; (CRYPTO-GOOD-DAY 20000) gives 22000
;(CRYPTO-GOOD-DAY 23450) gives 25795

;;! Part C

;; Define a function crypto-bad-day which simulates a single day of loss from
;; cryptocurrencies, assuming it was a bad day. Specifically, crypto-bad-day
;; should compute the total amount of money you will have after a –2% loss.
;; For example, if you start with $100 in crypto, after a bad day, you will
;; have $98 left. You must also write three examples for this function.

;; [TODO] Function definition

(define (CRYPTO-BAD-DAY CBD)
  (* CBD 0.98))

;; [TODO] Three examples
; (CRYPTO-BAD-DAY 100) gives 98
; (CRYPTO-BAD-DAY 200) gives 196
; (CRYPTO-BAD-DAY 150) gives 147 


;;! Part D

;; Define a constant STOCK-6-DAYS which is the total amount of money you would
;; have after starting with $1000 and repeatedly investing all of it in stocks
;; six days. You must use the stock-day function.
;; Hint: You can use the value produced by stock-day on the first day as the
;; argument for stock-day on the second day, and so on.

;; [TODO] Define the constant

(define STOCK-6-DAYS (STOCK-DAY (STOCK-DAY (STOCK-DAY (STOCK-DAY (STOCK-DAY (STOCK-DAY 1000)))))))
;The other method: (define STOCK-6-DAYS (* 1000 (expt 1.04 6)))(Don't know if this is also acceptable)

STOCK-6-DAYS

  



;; Define a constant CRYPTO-6-DAYS which simulates cryptocurrency trading for
;; 6 days starting with $1,000, alternating crypto-good-day and crypto-bad-day,
;; **starting with crypto-good-day**.

;; [TODO] Define the constant




(define CRYPTO-6-DAYS (CRYPTO-BAD-DAY (CRYPTO-GOOD-DAY (CRYPTO-BAD-DAY (CRYPTO-GOOD-DAY (CRYPTO-BAD-DAY (CRYPTO-GOOD-DAY 1000)))))))

CRYPTO-6-DAYS

;;! Part E

;; Now compare the results! Which one seems to have done better?

;; [TODO] Write which one seems better? Write it as a comment here.

; 1265>1252
; As a result, the stock-6-days's result is better than the result of crypto-6-day's


;; <file "hw1-problem3.rkt">

;;! Problem 3

;; In western classical music, tones are typically placed on a scale called
;; the twelve-tone scale. We use non-negative integers to refer to each tone.
;; For example 60 refers to the tone called "C" (or the "do" in "do-re-mi")
;; near the middle of a piano, whereas 61 refers to the tone one unit higher.
;; We consider two tones with a gap of a multiple of 12 units between them as
;; equivalent. For example, the tones 0, 60 and 84 are all equivalent: they are
;; all the tone "C". However, tones 60 and 67 are not equivalent.

;;! Part A

;; Define a function called tone-class which consumes a single tone as an
;; argument, and produces its *class*,  which is the smallest non-negative
;; integer that is equivalent to the tone. For example, the class of 60 is 0,
;; the class of 61 is 1, and the class of 0 is 0 itself. You must also write
;; three examples for your function.

;; Hint: Since there are 12 classes starting with zero, you can calculate the
;; class as the remainder. Try looking for relevant functions in the DrRacket
;; Help Desk.

           

;; [TODO] Function definition
(define (tone-class tone)
  (remainder tone 12))

;; [TODO] Three examples
;(tone-class 0) gives the result 0
; (tone-class 666) gives the result 6
;(tone-class 777) gives the result of 9

;;! Part B

;; The distance between two tones is how far apart they are, while keeping
;; equivalence in mind. Since there are 12 tone classes, the maximum distance
;; between any pair of tones is 12. However, there are two distances you can
;; produce, depending on which tone you consider first:

;; - The distance between tones 60 and 63 is either 3 (counting up) or 9
;;   (counting down).
;; - The distance between 60 and 75 is also either 3 or 9.
;; - The distance between 63 and 70 is 5 or 7.

;; Write a function called tone-distance which consumes two tones as arguments,
;; and produces their distance (either distance), as defined above.
;; Write three examples for tone-distance.

;; [TODO] Function definition

 (define (tone-distance tone1 tone2)
   (abs(- 12 (abs (- tone1 tone2)))))

 
;; [TODO] Thee examples
;(tone-distance 25 30) gives the result of 7
;(tone-distance 45 66) gives the result of 9
;(tone-distance 37 44) gives the result of 5



;;! Part C

;; On a piano keyboard, each class of twelve tones (a.k.a., an octave) are
;; placed in a standard pattern of eight white and five black keys. If you are
;; not familiar with this pattern, here is a picture of a piano keyboard:

;; https://en.wikipedia.org/wiki/Musical_keyboard#/media/File:Klaviatur-3-en.svg

;; Write a function called keyboard that consumes the height and width
;; of the white keys, and produces an image that looks like a piano octave.
;; The black keys are roughly half the width and about 3/4 the length of the
;; white keys.

;; Note: The picture linked above labels the white keys. Your image does not
;; have to do so.

;; Hint 1: The overlay/align function may be very helpful.

;; Hint 2: You can use "transparent" as a color for a rectangle



(define (white-key WIDTH HEIGHT)
  (rectangle WIDTH HEIGHT "outline" "black"))

(define (black-key WIDTH HEIGHT)
  (rectangle (* 0.25 WIDTH) (* 0.75 HEIGHT) "solid" "black"))
          

(define (keyboard WIDTH HEIGHT)
  (beside
   (overlay/align "right" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT))

   (overlay/align "right" "top" (black-key WIDTH HEIGHT) (overlay/align "left" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT)))

 
   (overlay/align "left" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT))               
   
   (overlay/align "right" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT))
   
   (overlay/align "right" "top" (black-key WIDTH HEIGHT) (overlay/align "left" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT)))
   
   (overlay/align "right" "top" (black-key WIDTH HEIGHT) (overlay/align "left" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT)))
   
   (overlay/align "left" "top" (black-key WIDTH HEIGHT)
   (white-key WIDTH HEIGHT))))
   

(keyboard 50 200)
                                                                 