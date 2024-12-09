;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Problem 1

(define-struct person [name next])
;;! A Line is one of:
;;! - "end of line"
;;! - (make-person String Line)
;;! Interpretation: A line of poeple.

(define LINE-EX-1 "end of line")
(define LINE-EX-2 (make-person "Alice" LINE-EX-1))
(define LINE-EX-3 (make-person "Alice" (make-person "Bob" LINE-EX-1)))

;;! Part A

;; Write the template for Line.

(define (line-temp le)
  (...
   (cond
     [(string? le)...]
     [(person? le)
      (person-name le)...
      (line-temp (person-next le))...])))
    


;;! Part B

;; Design a function called count-people that counts the number of people in
;; a line.

; count-people: Line -> Number
; Counts the number of people in a line



(check-expect (count-people LINE-EX-1) 0)
(check-expect (count-people LINE-EX-2) 1)
(check-expect (count-people LINE-EX-3) 2)

(define (count-people le)
  (cond
     [(string? le) 0]
     [(person? le)
      (+ 1 (count-people (person-next le)))]))


;;! Part C

;; Design a predicate called waldo-in-line? that determines if a person named
;; "Waldo" is in the line.

; waldo-in-line?: Line -> Boolean
; Determines if a person named "Waldo" is in the line



(check-expect (waldo-in-line? LINE-EX-1) #false)
(check-expect (waldo-in-line? LINE-EX-3) #false)
(check-expect (waldo-in-line? (make-person "Alice" (make-person "Waldo" LINE-EX-1))) #true)


(define (waldo-in-line? le)
  (cond
    [(string? le) #false]
    [(person? le)
      (or (string=? (person-name le) "Waldo")
          (waldo-in-line? (person-next le)))]))



;;! Part D

;; Design a function that removes the first "Waldo" in the line. It should have the
;; signature remove-waldo : Line -> Line.

; remove-waldo: Line -> Line
; removes the first "waldo" in the line


(check-expect (remove-waldo LINE-EX-1) LINE-EX-1)
(check-expect (remove-waldo LINE-EX-3) LINE-EX-3)

(check-expect (remove-waldo (remove-waldo (make-person "Alice" (make-person "Waldo" LINE-EX-1))))
              (make-person "Alice" LINE-EX-1))
                         
(check-expect (remove-waldo (make-person "Alice" (make-person "Waldo" (make-person "Waldo" LINE-EX-1))))
              (make-person "Alice" (make-person "Waldo" LINE-EX-1)))




(define (remove-waldo le)
  (cond
     [(string? le) le]
     [(person? le)
      (if
       (string=? (person-name le) "Waldo")
       (person-next le)
       (make-person (person-name le) (remove-waldo (person-next le))))]))
       
       
     

;;! Problem 2


(define-struct quest-entry [name completed next])
;;! A QuestLog is one of:
;;! - "empty"
;;! - (make-quest-entry String Boolean QuestLog)
;;! Interpretation: A quest log where each entry contains a quest name and a
;;! boolean that indicates if that quest is completed.

;;! Part A

;; Complete the data design for QuestLog (examples and template)

(define QUESTLOG-1 "empty")
(define QUESTLOG-2 (make-quest-entry "less" #true QUESTLOG-1))
(define QUESTLOG-3 (make-quest-entry "more" #false (make-quest-entry "large" #true QUESTLOG-1)))
(define QUESTLOG-4 (make-quest-entry "medium" #true QUESTLOG-3))


(define (questlog-temp qg)
  (...
   (cond
    [(string? qg)...]
    [(quest-entry? qg)
     (quest-entry-name qg)...
     (quest-entry-completed qg)...
     (questlog-temp (quest-entry-next qg))...])))
     



;;! Part B

;; Design a function called count-completed-quests that counts the number of
;; completed quests in a quest log.

; count-completed-quests: Questlog -> Number
; counts the number of completed quests in a quest log


(check-expect (count-completed-quests QUESTLOG-1) 0)
(check-expect (count-completed-quests QUESTLOG-2) 1)
(check-expect (count-completed-quests QUESTLOG-3) 1)
(check-expect (count-completed-quests QUESTLOG-4) 2)




(define (count-completed-quests qg)
  (cond
    [(string? qg) 0]
    [(quest-entry? qg)
     (if (quest-entry-completed qg)
         (+ 1 (count-completed-quests (quest-entry-next qg)))
         (count-completed-quests (quest-entry-next qg)))]))




;;! Part C

;; Design a function that consumes a QuestLog and only produces the incomplete
;; quests. It should have the signature incomplete-quests : QuestLog -> QuestLog.


; incomplete-quests : QuestLog -> QuestLog
; Consumes a QuestLog and only produces the incomplete quests

(check-expect (incomplete-quests QUESTLOG-1) "empty")
(check-expect (incomplete-quests QUESTLOG-2) "empty")
(check-expect (incomplete-quests QUESTLOG-3) (make-quest-entry "more" #false "empty"))
(check-expect (incomplete-quests QUESTLOG-4) (make-quest-entry "more" #false "empty"))



(define (incomplete-quests qg)
  (cond
    [(string? qg) qg]
    [(quest-entry? qg)
     (if (quest-entry-completed qg)
         (incomplete-quests (quest-entry-next qg))
         (make-quest-entry (quest-entry-name qg)
                           #false
                           (incomplete-quests (quest-entry-next qg))))]))





;;! Part D

;; Design a function that consumes a QuestLog and produces a new QuestLog with
;; the same quests, but all marked completed. Call the function mark-all-completed

; mark-all-completed: QuestLog -> QuestLog
; consumes a QuestLog and produces a new QuestLog with
; the same quests, but all marked completed


(check-expect (mark-all-completed QUESTLOG-1) "empty")
(check-expect (mark-all-completed QUESTLOG-2) (make-quest-entry "less" #true "empty"))

(check-expect (mark-all-completed QUESTLOG-3) (make-quest-entry "more" #true
                                                                (make-quest-entry "large" #true "empty")))

(check-expect (mark-all-completed QUESTLOG-4) (make-quest-entry "medium" #true
                                                                (make-quest-entry "more" #true
                                                                                  (make-quest-entry "large" #true "empty"))))



(define (mark-all-completed qg)
  (cond
    [(string? qg) qg]
    [(quest-entry? qg)
      (make-quest-entry (quest-entry-name qg)
                        #true
                        (mark-all-completed (quest-entry-next qg)))]))
      

       
                            
;;! Problem 3

;; This problem has a partially-completed data definition that represents a
;; workout sequence.

(define-struct cardio [rest])
(define-struct strength [rest])
(define-struct flexibility [rest])
;;! A Workout is one of:
;;! - (make-cardio Workout)
;;! - (make-strength Workout)
;;! - (make-flexibility Workout)
;;! - "done"
;;! Interpretation: A list of exercises in a long workout.

;;! Part A

;; Give three examples of Workouts.

(define WORKOUT-1 "done")
(define WORKOUT-2 (make-strength (make-flexibility WORKOUT-1)))
(define WORKOUT-3 (make-flexibility (make-cardio WORKOUT-1)))


;;! Part B

;; Write the template for Workouts.

(define (workout-temp wt)
  (...
   (cond
     [(string? wt)...]
     [(cardio? wt) (workout-temp (cardio-rest wt))...]
     [(strength? wt) (workout-temp (strength-rest wt))...]
     [(flexibility? wt) (workout-temp (flexibility-rest wt))...])))
     



;;! Part C

;; Design a function called recovery-sequence to generate the "recovery" sequence for a given
;; Workout. In the recovery sequence, cardio exercises become flexibility
;; exercises, strength exercises become cardio exercises, and flexibility
;; exercises become strength exercises.


; recovery-sequence: Workout -> Workout
; generate the workout sequence for a given workout


(check-expect (recovery-sequence WORKOUT-1) WORKOUT-1)
(check-expect (recovery-sequence WORKOUT-2) (make-cardio (make-strength WORKOUT-1)))
(check-expect (recovery-sequence WORKOUT-3) (make-strength (make-flexibility WORKOUT-1)))


(define (recovery-sequence wt)
  (cond
    [(string? wt) wt]
    [(cardio? wt) (make-flexibility (recovery-sequence (cardio-rest wt)))]
    [(strength? wt) (make-cardio (recovery-sequence (strength-rest wt)))]
    [(flexibility? wt) (make-strength (recovery-sequence (flexibility-rest wt)))]))



