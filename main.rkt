#lang racket

(require racket/format)
(require "matchmaker.rkt")
(require "sql-queries.rkt")

(define (get-config-file args)
  (cadr (member "-c" (vector->list args))))

(define (get-season-id args)
  (cadr (member "-s" (vector->list args))))

(define (get-matches-number args)
  (string->number (cadr (member "-m" (vector->list args)))))

(define (get-tries-number args)
  (string->number (cadr (member "-t" (vector->list args)))))

(define (get-limit args)
  (string->number (cadr (member "-l" (vector->list args)))))

(define (help? args)
  (list? (member "-h" (vector->list args))))

(define (display-help)
  (displayln (~a "utilisation : \n\t"
                 "-c <config-file>" "\n\t"
                 "-s <integer>" " : id de la saison\n\t"
                 "-m <integer>" " : nombre de matches\n\t"
                 "-t <integer>" " : nombre d'essais pour trouver la solution"
                 "-l <integer>" " : limite de matches par slot")))

(define (main args)
  (if (or (< (vector-length args) 10) (help? args))
      (display-help)
      (let ([config-file (get-config-file args)]
            [id-saison (get-season-id args)]
            [match-number (get-matches-number args)]
            [tries (get-tries-number args)]
            [max-matches-per-slot (get-limit args)])
        (let* ([pgc (db-access config-file)]
               [slots (get-slots pgc)]
               [teams (get-teams pgc id-saison slots)])
          (displayln `(found ,(length teams) teams and ,(length slots) slots))
          (let ([res (solve teams slots match-number tries max-matches-per-slot)])
            (displayln (cadr res))
            (add-matches pgc id-saison (car res)))))))

;; (main #("-c" "/home/noe/Téléchargements/sqlConfig.txt"
;;         "-s" "1"
;;         "-m" "5"
;;         "-t" "100000"
;;         "-l" "3"))

(main (current-command-line-arguments))
