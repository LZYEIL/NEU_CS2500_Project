;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
;; Purpose: Recipe recipe practice, now with structured data.

;;! Instructions
;;! 1. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 2. You must follow the _design recipe_ for every problem. In particular,
;;!    every function you define must have at least three check-expects (and
;;!    more if needed).
;;! 3. You must follow the Style Guide:
;;!    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;;! 4. You must submit working code. In DrRacket, ensure you get on errors
;;!    when you click Run. After you submit on Gradescope, you'll get instant
;;!    feedback on whether or Gradescope can run your code, and your code must
;;!    run on Gradescope to receive credit from the autograder.

;;! Problem 1

;; Consider the following data definition and interpretation.

(define-struct time (hours minutes seconds))
;;! A Time is a (make-time Number Number Number)
;;! Represents the time of day where:
;;! – hours is between 0 and 23
;;! – minutes is between 0 and 59
;;! – seconds is between 0 and 59

;;! Part A
;; Complete the two remaining parts of the data design for Time.

(define Time1 (make-time 0 20 30))
(define Time2 (make-time 2 59 59))
(define Time3 (make-time 23 59 59))


(define (time-template t)
  (...(time-hours t)
      (time-minutes t)
      (time-seconds t)...))
   


;;! Part B
;; Design a function called tick that adds one second to a Time.

; tick: Time -> Time
; Input a Time and update the Time by increasing one second

(check-expect (tick (make-time 0 0 0)) (make-time 0 0 1))
(check-expect (tick (make-time 0 0 59)) (make-time 0 1 0))
(check-expect (tick (make-time 0 59 59)) (make-time 1 0 0))
(check-expect (tick (make-time 23 59 59)) (make-time 0 0 0))


(define (tick t)
  (cond
    [(and (= (time-hours t) 23) (= (time-minutes t) 59) (= (time-seconds t) 59))
     (make-time 0 0 0)]
    
    [(and (< (time-hours t) 24) (< (time-minutes t) 59) (= (time-seconds t) 59))
     (make-time (time-hours t) (+ (time-minutes t) 1) 0)]
    
    [(and (< (time-hours t) 23) (= (time-minutes t) 59) (= (time-seconds t) 59))
     (make-time (+ (time-hours t) 1) 0 0)]
    
    [(and (< (time-hours t) 24) (< (time-minutes t) 59) (< (time-seconds t) 59))
     (make-time (time-hours t) (time-minutes t) (+ (time-seconds t) 1))]))



                          

;;! Part C

;; Design a function called time->image that draws an analog clock face with
;; three hands. (The hour hand is shortest and the minute and second hand should
;; be different.)
;;
;; See the link below for a refresher on how an analog clock works
;; https://en.wikipedia.org/wiki/Clock_face
;; Note: The hour hand does not need to base it's position on the minute hand
;; for this assignment

; time->image: Time -> Image
; purpose: The user inputs a Time and the program should
; output the image corresponds to the Time input



(define ClockFace (circle 100 "outline" "black"))
(define X-Central (* (image-width ClockFace) 0.5))
(define Y-Central (* (image-height ClockFace) 0.5))

(define SecondHand (add-line ClockFace X-Central Y-Central X-Central 10 "black"))
(define MinuteHand (add-line ClockFace X-Central Y-Central X-Central 20 "indigo"))
(define HourHand (add-line ClockFace X-Central Y-Central X-Central 40 "red"))

(define angle-for-hour -30)
(define angle-for-minute -6)
(define angle-for-second -6)



(check-expect (time->image (make-time 12 30 0)) (overlay
   (rotate (* (time-hours (make-time 12 30 0)) angle-for-hour) HourHand)
   (rotate (* (time-minutes (make-time 12 30 0)) angle-for-minute) MinuteHand)
   (rotate (* (time-seconds (make-time 12 30 0)) angle-for-second) SecondHand)))

(check-expect (time->image (make-time 14 35 6)) (overlay
   (rotate (* (time-hours (make-time 14 35 6)) angle-for-hour) HourHand)
   (rotate (* (time-minutes (make-time 14 35 6)) angle-for-minute) MinuteHand)
   (rotate (* (time-seconds (make-time 14 35 6)) angle-for-second) SecondHand)))

(check-expect (time->image (make-time 0 0 0)) (overlay
   (rotate (* (time-hours (make-time 0 0 0)) angle-for-hour) HourHand)
   (rotate (* (time-minutes (make-time 0 0 0)) angle-for-minute) MinuteHand)
   (rotate (* (time-seconds (make-time 0 0 0)) angle-for-second) SecondHand)))

(check-expect (time->image (make-time 5 59 59)) (overlay
   (rotate (* (time-hours (make-time 5 59 59)) angle-for-hour) HourHand)
   (rotate (* (time-minutes (make-time 5 59 59)) angle-for-minute) MinuteHand)
   (rotate (* (time-seconds (make-time 5 59 59)) angle-for-second) SecondHand)))


(define (time->image t)
  (overlay
   (rotate (* (time-hours t) angle-for-hour) HourHand)
   (rotate (* (time-minutes t) angle-for-minute) MinuteHand)
   (rotate (* (time-seconds t) angle-for-second) SecondHand)))






;;! Problem 2

;;! Part A

;; You are a feared restaurant critic whose ratings can make or break the
;; restaurants in Boston. Design a data definition called Review
;; that represents your review of a single restauant. A Review holds the
;; restaurant's name, its cuisine, the dish you ordered, its price, your
;; rating (1--5), and whether or not you saw any rats.

(define-struct review (name cuisine dish price rating rats?))

; A Review is a (make-review String String String Number Natural Boolean)
; Interpretation: Represents the review of a restaurant in Boston where:
; - name is its name
; - cuisine is the type of dishes
; - dish is the ordered dishes of customers 
; - price is the dishes' costs
; - rating is my rating of the restaurant from 1 to 5
; - rats? is whether or not the customer sees any rats in the restaurant

(define review1 (make-review "kfc" "fast food" "hamburger" 5 1 #false))
(define review2 (make-review "kungfu" "chinese" "gongbao chicken" 4 5 #false))
(define review3 (make-review "tatte" "take-away desert" "cake" 3 4 #true))
(define review4 (make-review "haidilao" "hotpot" "tomato hotpot" 20 3 #false))
(define review5 (make-review "qiaojiangnan" "yue dish" "chicken wings" 30 2 #true))



(define (review-template re)
  (...
   (review-name re)
   (review-cuisine re)
   (review-dish re)
   (review-price re)
   (review-rating re)
   (review-rats? re)...))
   
  


;;! Part B

;; Design a function called update-rating that takes a Review and a new rating,
;; and updates the review with the new rating.

; update-rating: Review Natural -> Review
; Purpose: takes a review from me with a new rating and
; it should print the updated version


(check-expect (update-rating review1 3)
              (make-review "kfc" "fast food" "hamburger" 5 3 false))

(check-expect (update-rating review2 4)
              (make-review "kungfu" "chinese" "gongbao chicken" 4 4 false))

(check-expect (update-rating review3 6)
              "invalid")

(check-expect (update-rating review4 -3)
              "invalid")



(define (update-rating re new-rating )
  (if
   (and (< new-rating 6) (> new-rating 0))
   (make-review (review-name re) (review-cuisine re) (review-dish re) (review-price re)
               new-rating (review-rats? re))
   "invalid"))


              

;;! Part C

;; Design a function called rat-sighting that takes a Review and marks it as
;; a restaurant with rats. It also decreases its rating by 1 star, only if
;; the restaurant was not previously known to have rats.

; rat-sighting: Review -> Review
; the purpose: marks the restaurant with rats and decrease its rating by 1 star if
; it had the rats in the past



(check-expect (rat-sighting review1) "invalid")
(check-expect (rat-sighting review2) (make-review "kungfu" "chinese" "gongbao chicken" 4 4 #true))
(check-expect (rat-sighting review3) review3)
(check-expect (rat-sighting review4) (make-review "haidilao" "hotpot" "tomato hotpot" 20 2 #true))
(check-expect (rat-sighting review5) (make-review "qiaojiangnan" "yue dish" "chicken wings" 30 2 #true))



             
(define (rat-sighting re)
  (cond
    [(review-rats? re)
    (make-review (review-name re) (review-cuisine re) (review-dish re) (review-price re) (review-rating re) (review-rats? re))]
    [(and (not (review-rats? re)) (>= (review-rating re) 2))
     (make-review (review-name re) (review-cuisine re) (review-dish re) (review-price re) (- (review-rating re) 1) true)]
    [else "invalid"]))
   

     


;;! Problem 3

;; You are in the robot part business, making essential parts for robots.
;; The only parts you make are LIDAR sensors, depth cameras, accelerometers,
;; electric motors, and batteries. For every part, you track the kind of
;; part, the price of the item, and the number of items in stock.


;;! Part A
;; Design data definitions called PartType and Stock to represent
;; a single type of item in stock.

;PartType is one of:
; - "LIDAR sensors"
; - "depth cameras"
; - "accelerometers"
; - "electric motors"
; - "batteries"

; Interpretation: A PartType is each type of products
; the business makes

(define PT1 "LIDAR sensors")
(define PT2 "depth sensors")
(define PT3 "accelerometers")
(define PT4 "electric motors")
(define PT5 "batteries")

(define (pt-template pt)
  (...(cond
        [(string=? pt PT1)...]
        [(string=? pt PT2)...]
        [(string=? pt PT3)...]
        [(string=? pt PT4)...]
        [(string=? pt PT5)...])))


(define-struct stock (part price stockNum))
;A stock is (make-stock PartType Number Integer)
;Interpretation: represents the products' information of the business where:
; - part is one of:
; - "LIDAR sensors"
; - "depth cameras"
; - "accelerometers"
; - "electric motors"
; - "batteries"
; - price is the product's price
; - stockNum is the quantity of the product

(define ST1 (make-stock "LIDAR sensors" 50 500))
(define ST2 (make-stock "depth cameras" 30 400))
(define ST3 (make-stock "accelerometers" 35 450))
(define ST4 (make-stock "electric motors" 75 800))
(define ST5 (make-stock "batteries" 63 654))


(define (st-template st)
  (...
   (stock-part st)
   (stock-price st)
   (stock-stockNum st)...))


   

;;! Part B

;; Design a function called discount that takes an Stock and a discount
;; value, and reduces the price by the given value. However, if the price
;; is lower than $10, do not apply the discount. You can assume that the
;; discount is less than the original price.

; discount: Stock Number -> Stock
; purpose: takes an stock and discount and apply the discount to the product


(check-expect (discount ST1 4) (make-stock "LIDAR sensors" 46 500))
(check-expect (discount (make-stock "electric motors" 5 30) 6) (make-stock "electric motors" 5 30))
(check-expect (discount ST2 -8) "invalid")


                         
(define (discount st dis)
  (cond
    [(and (> dis 0) (< (stock-price st) 10))
    (make-stock (stock-part st) (stock-price st) (stock-stockNum st))]
    [(and (> dis 0) (>= (stock-price st) 10))
    (make-stock (stock-part st) (- (stock-price st) dis) (stock-stockNum st))]
   [else
    "invalid"]))
    
   


;;! Part C

;; Design a function called greater-value? that takes two Stocks and
;; produces #true iff the value (quantity * price) of the first is greater than
;; or equal to the value of the second.
;; Note: To receive full credit, you will need to write a helper function that
;; follows the template.

; Helper function: (should be defined before its use)
; calculate-value: Stock -> Number
; purpose: Takes in the Stock and calculate its
; price * number of quantity in order to get the 'value' 


(check-expect (calculate-value ST1) 25000)
(check-expect (calculate-value ST2) 12000)
(check-expect (calculate-value ST5) 41202)


(define (calculate-value st)
  (* (stock-price st) (stock-stockNum st)))


; greater-value?: Stock Stock -> Boolean
; purpose: Takes 2 stocks and determine if the first stock's value is equal
; or greater than the second's

(check-expect (greater-value? ST2 ST1) false)
(check-expect (greater-value? ST1 ST2) true)
(check-expect (greater-value? ST3 ST1) false)

              
(define (greater-value? st1 st2)
  (>= (calculate-value st1) (calculate-value st2)))























