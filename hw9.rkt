;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2023F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;; 2. When we write "complete the following function design", you should      ;;
;;    write the function definition and check-expects.                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! Problem 1

;; Complete the following function design.

;;! average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;;! Produces a list of numbers where each number is the average of the
;;! corresponding numbers in the two lists.


(check-expect (average-of-two-lists (list 1 2 3) (list 3 2 1)) (list 2 2 2))
(check-expect (average-of-two-lists '() (list 1 2 3)) '())
(check-expect (average-of-two-lists (list 1 2 3) '()) '())
(check-expect (average-of-two-lists (list 0 2 3 4) (list 3 2 1)) (list 1.5 2 2))



(define (average-of-two-lists lon1 lon2)
  (cond
    [(or (empty? lon1) (empty? lon2)) '()]
    [(and (cons? lon1) (cons? lon2))
     (cons (/ (+ (first lon1) (first lon2)) 2) (average-of-two-lists (rest lon1) (rest lon2)))]))
     


;;! Problem 2

;; Complete the following function design *without using the builtin function
;; replicate*.

;;! repeat-strings-solo : Nat [List-of String] -> [List-of String]
;;! (repeat-strings-solo n slist) produces a produces a list of strings where
;;! each output string is the corresponding input string repeated n times.


(check-expect (repeat-strings-solo 4 (list "l" "m" "tu")) (list "llll" "mmmm" "tutututu"))
(check-expect (repeat-strings-solo 7 (list "a" "b")) (list "aaaaaaa" "bbbbbbb"))
(check-expect (repeat-strings-solo 7 '()) '())



(define (repeat-strings-solo n slist)
  (local
    [(define (string-repeat n str)
       (if (<= n 0)
           ""
           (string-append str (string-repeat (- n 1) str))))]
     (cond
       [(empty? slist) '()]
       [(cons? slist)
        (cons (string-repeat n (first slist))
              (repeat-strings-solo n (rest slist)))])))



;;! Problem 3

;; Complete the following function design *and you may use the builtin
;; replicate.*

;;! repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;;! (repeat-strings slist nlist) produces a list of strings from slist, where
;;! each is duplicated N times, where N is the corresponding number in
;;! nlist. However:
;;!
;;! 1. If there  are more strings than numbers, assume that the extra strings
;;!    should be repeated twice each.
;;! 2. If there are more numbers than strings, for each extra number N,
;;!    repeat the the string "Extra!" N times.


(check-expect (repeat-strings (list "a" "b") (list 2 3)) (list "aa" "bbb"))
(check-expect (repeat-strings (list "a" "b" "c") (list 2 3)) (list "aa" "bbb" "cc"))
(check-expect (repeat-strings (list "a") (list 1 2 3)) (list "a" "Extra!Extra!" "Extra!Extra!Extra!"))




(define (repeat-strings slist nlist)
  (cond
    [(and (empty? slist) (empty? nlist)) '()]
    [(and (empty? slist) (cons? nlist))
     (cons (replicate (first nlist) "Extra!") (repeat-strings slist (rest nlist))) ]
    [(and (cons? slist) (empty? nlist))
     (cons (replicate 2 (first slist)) (repeat-strings (rest slist) nlist))]
    [(and (cons? slist) (cons? nlist))
     (cons (replicate (first nlist) (first slist)) (repeat-strings (rest slist) (rest nlist)))]))
  


;;! Problem 4

;; Consider the following data definitions (we have omitted examples and
;; templates).

(define-struct student [name nuid])
;;! A Student is a (make-student String Number)
;;! Interpretation: represents a student


(define-struct grade [nuid course value])
;;! A Grade is a (make-grade Number String Number)
;;! (make-grade nuid course grade) represents the grade that
;;! a student received in a course.

(define-struct student-grades [name grades])
;;! A StudentGrades is a (make-student-grades String [List-of Number]).
;;! (make-student-grades name grades) represents the grades
;;! that a student has received in all courses.

;; Complete the following function design.

;;! students->student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;;! Produces a StudentGrade for each student, with the list of grades that
;;! student received. The list produced should have an item for every student in the
;;! input list, even if there are no grades for that student.



(check-expect (students->student-grades '() '()) '())

(check-expect (students->student-grades (list (make-student "Alice" 1780) (make-student "Bob" 890)) '())
              (list (make-student-grades "Alice" '()) (make-student-grades "Bob" '())))


(check-expect (students->student-grades '()
                                        (list (make-grade 001 "Math" 85) (make-grade 002 "Chemistry" 90)))
              '())


(check-expect (students->student-grades (list (make-student "Alice" 1780) (make-student "Bob" 890))
                                        (list (make-grade 1780 "Math" 99) (make-grade 890 "English" 40) (make-grade 004 "Chemistry" 90)))
              (list (make-student-grades "Alice" (list 99)) (make-student-grades "Bob" (list 40))))


(check-expect (students->student-grades (list (make-student "Alice" 1780) (make-student "Bob" 890))
                                        (list (make-grade 1780 "Math" 99)
                                              (make-grade 1780 "English" 34)
                                              (make-grade 890 "Chemistry" 62)
                                              (make-grade 1780 "CS2500" 0)))
              (list (make-student-grades "Alice" (list 99 34 0)) (make-student-grades "Bob" (list 62))))






(define (students->student-grades los log)
  (cond
    [(empty? los) '()]
    [(and (cons? los) (empty? log))
     (cons (make-student-grades (student-name (first los)) '()) (students->student-grades (rest los) log))]
    [(and (cons? los) (cons? log))
     (cons (make-student-grades (student-name (first los))
                                (map grade-value (filter (lambda (g) (= (student-nuid (first los)) (grade-nuid g))) log)))
           (students->student-grades (rest los) (rest log)))]))
  



