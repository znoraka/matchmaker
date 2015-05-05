#lang racket
(require db)
(require "matchmaker.rkt")

(define (db-access config-file)
  (define in (open-input-file config-file))
  
  (define (read-info)
    (cadr (string-split (read-line in) "=")))

  (define (connection-infos s)
    (let* ([s (string-split s ":")]
           [server (car s)]
           [s (string-split (cadr s) "/")]
           [port (car s)]
           [db (cadr s)])
      (list server (string->number port) db)))
  
  (let ([c (connection-infos (read-info))])
    (mysql-connect #:server (car c)
                   #:user (read-info)
                   #:password (read-info)
                   #:port (cadr c)
                   #:database (caddr c))))

(define (get-slots pgc)
  (for/list ([i (query-rows pgc (~a "SELECT * "
                                    "FROM rush_4v4_timeslots "
                                    "WHERE enable = 1"))])
             (vector-ref i 0)))

(define (get-teams pgc id-saison)
  (define (get-slots-for-team id-team)
    (vector->list (query-rows pgc (~a "SELECT idSlot "
                                      "FROM rush_4v4_timeslots_selected "
                                      "WHERE idTeam = " id-team))))
    
  (for/list ([i (query-rows pgc (~a "SELECT * "
                                    "FROM rush_4v4_registedteams "
                                    "WHERE idSaison = " id-saison))])
    (let ([id (vector-ref i 1)])
      (make-team id (get-slots-for-team id)))))

(define (add-match pgc id-saison m)
  
  

(define (test)
  (db-access "/home/noe/Téléchargements/sqlConfig.txt"))
