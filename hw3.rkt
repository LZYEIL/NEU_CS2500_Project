;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Purpose: Recipe recipe practice, now with unions and self-referential data definitions.

(require 2htdp/image)
(require 2htdp/universe)

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

;; Consider the following structure definitions:
(define-struct blender [brand wattage crushes-ice?])
(define-struct microwave [brand power-level])
(define-struct kettle [brand capacity])
(define-struct toaster [brand slices])

;;! Part A

;; Complete four data designs for each structure called Blender, Microwave,
;; Kettle, and Toaster.


; A blender is (make-blender String Number Boolean)
; Interpretation: Represents the appliance called the blender which:
; - brand is its brand
; - wattage is its power unit 
; - crushes-ice? is to check whether or not it crushes the ice

(define BLENDER1 (make-blender "nb" 80 false))
(define BLENDER2 (make-blender "km" 60 true))

(define (blender-temp br)
  (...
   (blender-brand br)...
   (blender-wattage br)...
   (blender-crushes-ice? br)...))



; A microwave is (make-microwave String Number)
; Interpretation: Represents the applicance called the microwave which:
; - brand is its brand
; - power-level is the level of power it operates

(define MICROWAVE1 (make-microwave "kt" 90))
(define MICROWAVE2 (make-microwave "ty" 75))

(define (microwave-temp me)
  (...
   (microwave-brand me)...
   (microwave-power-level me)...))





; A kettle is (make-kettle String Number)
; Interpretation: Represents the appliance called the kettle which:
; - brand is its brand
; - capacity is its volume of liquid

(define KETTLE1 (make-kettle "ol" 50))
(define KETTLE2 (make-kettle "pi" 95))

(define (kettle-temp ke)
  (...
   (kettle-brand ke)...
   (kettle-capacity ke)...))





; A toaster is (make-toaster String Natural)
; Interpretation: Represents the appliance called the toaster which:
; - brand is its brand
; - slices is the number of bread in the toaster

(define TOASTER1 (make-toaster "ld" 5))
(define TOASTER2 (make-toaster "yr" 10))

(define (toaster-temp tr)
  (...
   (toaster-brand tr)...
   (toaster-sllices tr)...))


;;! Part B

;; Complete a data design called Appliance, which can represent any appliance
;; listed above.

; Appliance is one of:
; - Blender
; - Microwave
; - Kettle
; - Toaster
; Interpretation: represents all existing appliances

(define APPLIANCE1 BLENDER1)
(define APPLIANCE2 MICROWAVE1)
(define APPLIANCE3 KETTLE1)
(define APPLIANCE4 TOASTER1)

(define (appliance-temp ae)
  (...(cond
        [(blender? ae) (blender-temp ae)...]
        [(microwave? ae) (microwave-temp ae)...]
        [(kettle? ae) (kettle-temp ae)...]
        [(toaster? ae) (toster-temp ae)...])))


 


;;! Part C

;; Complete a data design called Kitchen, which may have 1--3 appliances.
;; (If you have read ahead to lists, do not use lists.)

; An ApplianceTwo is one of:
; #false
; Appliance
;Interpretation: represents false or Appliance


(define APPLIANCETWO1 #false)
(define APPLIANCETWO2 APPLIANCE2)
(define APPLIANCETWO3 APPLIANCE3)

(define (applianceTwo-temp a2)
  (... (cond
         [(boolean? a2)...]
         [else (appliance-temp a2)...])))


(define-struct kitchen [first second third])

; A kitchen is a (make-kitchen Appliance ApplianceTwo ApplianceTwo)
; Interpretation: A kitchen contains one to three appliances

(define KITCHEN1 (make-kitchen APPLIANCE1 APPLIANCETWO1 APPLIANCETWO2))
(define KITCHEN2 (make-kitchen APPLIANCE3 APPLIANCETWO2 APPLIANCETWO2))
(define KITCHEN3 (make-kitchen APPLIANCE4 APPLIANCETWO1 APPLIANCETWO1))


(define (kitchen-temp k)
  (...
   (kitchen-first k)...
   (kitchen-second k)...
   (kitchen-third k)...))


    


;;! Part D

;; Design a function that takes a Kitchen and produces another Kitchen
;; that is identical, except that all microwaves have their power-level
;; incremented by 50.



; appliance-helper: Appliance -> Appliance
; takes a appliance and determine if it is the microwave,
; if it is then add power level by 50, otherwise remain the same


(check-expect (appliance-helper APPLIANCE1) APPLIANCE1)
(check-expect (appliance-helper APPLIANCE3) APPLIANCE3)
(check-expect (appliance-helper APPLIANCE2) (make-microwave "kt" 140))

(define (appliance-helper ae)
  (cond
   [(microwave? ae) (make-microwave
                     (microwave-brand ae) (+ (microwave-power-level ae) 50))]
                [else ae]))


; applianceTwo-helper: ApplianceTwo -> ApplianceTwo
; input a applianceTwo and check whether it is a boolean or
; appliance. If it is a boolean then output the input, otherwise call the
; helper function of the appliance


(check-expect (applianceTwo-helper APPLIANCETWO1) APPLIANCETWO1)
(check-expect (applianceTwo-helper APPLIANCETWO2) (make-microwave "kt" 140))
(check-expect (applianceTwo-helper APPLIANCETWO3) APPLIANCETWO3)


(define (applianceTwo-helper a2)
  (cond
         [(boolean? a2) a2]
         [else (appliance-helper a2)]))
                
                

;kitchen-operate: Kitchen -> Kitchen
; takes in a kitchen and produces another identical chicken, but increase
; level of the microwave's power level by 50 if it exists

(check-expect (kitchen-operate KITCHEN1) (make-kitchen
                                          (make-blender "nb" 80 #false)
                                          #false
                                          (make-microwave "kt" 140)))

(check-expect (kitchen-operate KITCHEN2) (make-kitchen
                                          (make-kettle "ol" 50)
                                          (make-microwave "kt" 140)
                                          (make-microwave "kt" 140)))

(check-expect (kitchen-operate KITCHEN3) KITCHEN3)


      
(define (kitchen-operate en)
  (make-kitchen (appliance-helper (kitchen-first en))
                (applianceTwo-helper (kitchen-second en))
                (applianceTwo-helper (kitchen-third en))))
   




;;! Problem 2

;; You work at a vehicle dealership, and you need to keep track of different
;; types of vehicles: cars, motorcycles, and trucks. For each car, you track
;; its brand, mileage, and number of seats. For each motorcycle, you track its
;; brand, mileage, and engine size. For each truck, you track its brand, mileage
;; and payload capacity.

;;! Part A

;; Complete a data design called Vehicle that can represent any one vehicle.

(define-struct cars (brand mileage num-of-seats))
; A cars is (make-cars String Number Number)
; represents a car where:
; - brand is its brand
; - mileage is the mile it can run
; - num-of-seats is the number of seats of it

(define CARS1 (make-cars "toyota" 100 5))
(define CARS2 (make-cars "honda" 400 3))

(define (cars-template cr)
  (...
   (cars-brand cr)...
   (cars-mileage cr)...
   (cars-num-of-seats cr)...))
  



(define-struct motorcycle (brand mileage engine-size))
; A motorcycle is (make-motorcycle String Number Number)
; represents a motorcycle where:
; - brand is its brand
; - mileage is the mile it can run
; - engine-size is the size of the engine

(define MOTORCYCLE1 (make-motorcycle "Chuanqi" 50 5))
(define MOTORCYCLE2 (make-motorcycle "BMW" 100 10))

(define (motorcycle-template motor)
  (...
   (motorcycle-brand motor)...
   (motorcycle-mileage motor)...
   (motorcycle-engine-size motor)...))





(define-struct truck (brand mileage payload))
; A truck is (make-truck String Number Number)
; represents a truck where:
; - brand is its brand
; - mileage is the mile it can run
; - payload is the total weight of cargo it can carry


(define TRUCK1 (make-truck "volvo" 400 300))
(define TRUCK2 (make-truck "Scanvia" 500 250))

(define (truck-template trk)
  (...
   (truck-brand trk)...
   (truck-mileage trk)...
   (truck-payload trk)...))






; Vehicle is one of:
; - Cars
; - Motorcycle
; - Truck
;Interpretation: represents all existing transpoatation tools

(define VEHICLE1 CARS1)
(define VEHICLE2 MOTORCYCLE1)
(define VEHICLE3 TRUCK1)


(define (vehicle-template vh)
  (...(cond
        [(cars? vh) (cars-template vh)...]
        [(motorcycle? vh) (motorcycle-template vh)...]
        [(truck? vh) (truck-template vh)...])))




;;! Part B

;; Design a predicate called `high-mileage?` that determines if a vehicle has
;; is high mileage. Trucks are high-mileage if they have completed more than
;; 250,000 miles, but the others are high-mileage if they have completed more
;; than 100,000 miles.


;high-mileage? : Vehicle -> Boolean
; determines if each vehicle is considered as
; high mileage based on their miles run


(check-expect (high-mileage? CARS1) #false)
(check-expect (high-mileage? MOTORCYCLE1) #false)
(check-expect (high-mileage? TRUCK1) #false)
(check-expect (high-mileage? (make-truck "volvo" 300000 100)) #true)
(check-expect (high-mileage? (make-cars "volvo" 200000 5)) #true)


              


(define (high-mileage? vh)
  (cond
    [(cars? vh) (> (cars-mileage vh) 100000)]
    [(motorcycle? vh) (> (motorcycle-mileage vh) 100000)]
    [(truck? vh) (> (truck-mileage vh) 250000)]))


    

;;! Part C

;; Design a function with the following signature and purpose statement:

;;! add-miles : Vehicle Number -> Vehicle
;;! Adds the given number of miles to the vehicle's mileage.


; add-miles : Vehicle Number -> Vehicle
; Adds the given number of miles to the vehicle's mileage.


(check-expect (add-miles CARS1 50) (make-cars "toyota" 150 5))
(check-expect (add-miles TRUCK1 500) (make-truck "volvo" 900 300))
(check-expect (add-miles MOTORCYCLE1 40) (make-motorcycle "Chuanqi" 90 5))



(define (add-miles vh num)
  (cond
    [(cars? vh) (make-cars (cars-brand vh)
                          (+ num (cars-mileage vh))
                          (cars-num-of-seats vh))]
    [(motorcycle? vh) (make-motorcycle (motorcycle-brand vh)
                                       (+ num (motorcycle-mileage vh))
                                       (motorcycle-engine-size vh))]
    [(truck? vh) (make-truck (truck-brand vh)
                             (+ num (truck-mileage vh))
                             (truck-payload vh ))]))



;;! Part D

;; Design a function called `describe-vehicle` takes a `Vehicle` and
;; produces one of these strings:
;; - "A car that seats <n> people!"
;; - "A motorcycle with a <n>cc engine!"
;; - "A truck that hauls <n>lbs!"

; describe-vehicle : Vehicle -> String
;; output the passenger number of the car OR engine size of motorcycle
; OR payload capacity of truck



(check-expect (describe-vehicle MOTORCYCLE1) "A motorcycle with a 5cc engine!")
(check-expect (describe-vehicle CARS1) "A car that seats 5 people!")
(check-expect (describe-vehicle TRUCK1) "A truck that hauls 300lbs!")

(define (describe-vehicle vh)
  (cond
    [(cars? vh) (string-append "A car that seats " (number->string (cars-num-of-seats vh)) " people!")]
    [(motorcycle? vh) (string-append "A motorcycle with a " (number->string (motorcycle-engine-size vh)) "cc engine!")]
    [(truck? vh) (string-append "A truck that hauls " (number->string (truck-payload vh)) "lbs!")]))



;;! Problem 3

;; Write a world program that looks and behaves approximately like this:
;;
;; https://pages.github.khoury.northeastern.edu/2500/2023F/starter/hw3_demo.gif
;;
;; The two triangles must be oriented as shown, and they must follow the mouse
;; as shown. Beyond that, feel free to be creative.
;;
;; Your world program should have the following name and signature:

;; target-program : WorldState -> WorldState
;; (define (target-program initial-state)
;;  (big-bang initial-state
;;    ...))

;; (Recall that big-bang produces the final State.)
;;
;; Furthermore:
;; 1. You can define WorldState however you like.
;; 2. When you click Run, the window must *not* appear. i.e., use
;; target-program in Interactions, and not in Definitions.


(define SCENE (square 400 "outline" "black"))

(define Triangle1 (rotate 180 (triangle 40 "solid" "red")))
(define Triangle2 (rotate 270 (triangle 40 "solid" "red")))


; draw-triangle: Posn -> Image
; place the image (triangle) to the position input

(check-expect (draw-triangle (make-posn 20 50)) (place-image Triangle1 20 20
               (place-image Triangle2 20 50 SCENE)))

(check-expect (draw-triangle (make-posn 80 50)) (place-image Triangle1 80 20
               (place-image Triangle2 20 50 SCENE)))

(check-expect (draw-triangle (make-posn 100 100)) (place-image Triangle1 100 20
               (place-image Triangle2 20 100 SCENE)))



(define (draw-triangle p)
  (place-image Triangle1 (posn-x p) 20
               (place-image Triangle2 20 (posn-y p) SCENE)))



; mouse-action: Posn Real Real String -> Posn
; Input the current position of the mouse and the new x/y coordinates
; of the mouse, the output should be a new position of mouse 

(check-expect (mouse-action (make-posn 20 20) 40 40 "move") (make-posn 40 40))
(check-expect (mouse-action (make-posn 30 30) 50 50 "move") (make-posn 50 50))
(check-expect (mouse-action (make-posn 30 40) 40 30 "move") (make-posn 40 30))

(define (mouse-action p x y mevent)
  (make-posn x y))


; traget-program: Posn -> Posn
(define (target-program initial-state)
   (big-bang initial-state
     (to-draw draw-triangle)
     (on-mouse mouse-action)))



