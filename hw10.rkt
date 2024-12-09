;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct manager [name team])
(define-struct ic [name])
(define-struct team [name members])

;;! A Manager is a (make-manager String Team)
;;! Interpretation: a manager at a company who directly manages the team.

;;! A Person is one of:
;;! - (make-manager String Team)
;;! - (make-ic String)
;;! Interpretation: A person at a company who is either a manager or an an
;;! individual contributor ("IC").

;;! A Team is a (make-team String [List-of Person])
;;! Interpretation: A team at a company, with a name and a list of members.

;;! Problem 1

;; Define three examples of Manager. One of them should have someone with exactly
;; the same name at two levels of the hierarchy, because that is a common source
;; of confusion at large companies.

(define MANAGER-1 (make-manager "Thomas" (make-team "Team A" (list (make-ic "Calvin") (make-ic "Kevin")))))
(define MANAGER-2 (make-manager "Aurora" (make-team "Team B" (list MANAGER-1 (make-ic "Tom")))))
(define MANAGER-3 (make-manager "Thomas" (make-team "Team C" (list MANAGER-2 (make-ic "Jack")))))


;;! Problem 2

;; Complete the following function design.

;;! list-direct-reports : String Manager -> [List-of String]
;;! Produces a list of all the direct reports of the manager with the given
;;! name. When several managers have the same name, all of them are included.


(check-expect (list-direct-reports "Thomas" MANAGER-1) (list "Calvin" "Kevin"))
(check-expect (list-direct-reports "Thomas" MANAGER-2) (list "Calvin" "Kevin"))
(check-expect (list-direct-reports "Thomas" MANAGER-3) (list "Aurora" "Jack" "Calvin" "Kevin"))
(check-expect (list-direct-reports "Aurora" MANAGER-2) (list "Thomas" "Tom"))


; so when you are given a manager, you should check if the manager's name matches the given name.
; If it does,  you should include the manager's team's names in your result,
; and then call the function recursively on any person in the manager's team that is also a manager.
; If the name does not match, then you should still call the function recursively
; on anyone in the team that is a manager, but you would not include the current manager's team's names.


(define (list-direct-reports str mr)
  (local
    [(define MODIFIED-LIST (map (lambda (p)
                                  (if (manager? p)
                                      (manager-name p)
                                      (ic-name p))) (team-members (manager-team mr))))
     
     (define RECURSIVE-LIST (foldr (lambda (p lon)
                                     (if (manager? p)
                                         (append (list-direct-reports str p) lon)
                                         lon)) '() (team-members (manager-team mr))))]
    (if (string=? str (manager-name mr))
        (append MODIFIED-LIST RECURSIVE-LIST)
        RECURSIVE-LIST)))
      
          
                      
;;! Problem 3

;; Complete the following function design. Hint: this requires an accumulator

;;! list-managers : String Manager -> [List-of String]
;;! Produces a list of all the managers who directly manage someone with the
;;! given name. When several people have the same name, list all their managers.


(check-expect (list-managers "Kevin" MANAGER-1) (list "Thomas"))
(check-expect (list-managers "Aurora" MANAGER-1) '())
(check-expect (list-managers "Thomas" MANAGER-3) (list "Aurora"))



(define (list-managers str mr)
  (local
    [(define MODIFIED-LIST (filter string? (map (lambda (p) (if (ic? p)
                                                                (if (string=? str (ic-name p))
                                                                    (manager-name mr)
                                                                    #false)
                                                                (if (string=? str (manager-name p))
                                                                    (manager-name mr)
                                                                    #false))) (team-members (manager-team mr)))))
      
     (define RECURSIVE-LIST (foldr (lambda (p lon)
                                     (if (manager? p)
                                         (append (list-managers str p) lon)
                                         lon)) '() (team-members (manager-team mr))))]

    (append MODIFIED-LIST RECURSIVE-LIST)))
 
                                     
