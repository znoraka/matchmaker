#lang racket

(require "matchmaker.rkt")
(require "sql-queries.rkt")

(define (main params)
  (let ([config-file (vector-ref params 0)]
        [id-saison (vector-ref params 1)]
        [match-number (string->number (vector-ref params 2))]
        [tries (string->number (vector-ref params 3))])
    (let* ([pgc (db-access config-file)]
           [teams (get-teams pgc id-saison)]
           [slots (get-slots pgc)])
      (let ([res (solve teams slots match-number tries)])
        (displayln (~a "found " (cadr res) " matches"))
        (add-matches pgc id-saison (car res))))))

(main #("/home/noe/Téléchargements/sqlConfig.txt"
        "1"
        "5"
        "1000"))

(define (generate-random-data teams-number id-saison)
  (let* ([pgc (db-access "/home/noe/Téléchargements/sqlConfig.txt")]
         [slots (get-slots pgc)]
         [teams (make-n-random-teams slots teams-number)])
      (insert-teams pgc id-saison teams)
      (insert-slots pgc teams)))
