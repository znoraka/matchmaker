#lang racket

(require "matchmaker.rkt")
(require "sql-queries.rkt")

(define (get-config-file args)
  (cadr (member "-i" (vector->list args))))

(define (get-season-id args)
  (cadr (member "-s" (vector->list args))))

(define (get-matches-number args)
  (string->number (cadr (member "-m" (vector->list args)))))

(define (get-tries-number args)
  (string->number (cadr (member "-t" (vector->list args)))))

(define (help? args)
  (list? (member "-h" (vector->list args))))

(define (display-help)
  (displayln (~a "utilisation : \n\t"
                 "-i <config-file>" "\n\t"
                 "-s <integer>" " : id de la saison\n\t"
                 "-m <integer>" " : nombre de matches\n\t"
                 "-t <integer>" " : nombre d'essais pour trouver la solution")))

(define (main args)
  (if (or (< (vector-length args) 8) (help? args))
      (display-help)
      (let ([config-file (get-config-file args)]
            [id-saison (get-season-id args)]
            [match-number (get-matches-number args)]
            [tries (get-tries-number args)])
        (let* ([pgc (db-access config-file)]
               [slots (get-slots pgc)]
               [teams (get-teams pgc id-saison slots)])
          (displayln `(found ,(length teams) teams and ,(length slots) slots))
          (let ([res (solve teams slots match-number tries)])
            (displayln (cadr res))
            (add-matches pgc id-saison (car res)))))))

;; (main #("/home/noe/Téléchargements/sqlConfig.txt"
;;         "1"
;;         "5"
;;         "100000"))

(main (current-command-line-arguments))
