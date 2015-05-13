#lang racket

(require racket/format)
(require db)
(require "matchmaker.rkt")

(provide db-access)
(provide get-slots)
(provide get-teams)
(provide add-matches)

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

(define (get-teams pgc id-saison all-slots)
  (define (get-slots-for-team id-team)
    (let ([res (query-rows pgc (~a "SELECT idSlot "
                                      "FROM rush_4v4_timeslots_selected "
                                      "WHERE idTeam = " id-team))])
      (filter (λ (i)
                (member i all-slots))
              (map (λ (i)
                     (vector-ref i 0)) res))))
    
  (for/list ([i (query-rows pgc (~a "SELECT * "
                                    "FROM rush_4v4_registedteams "
                                    "WHERE idSaison = " id-saison))])
    (let ([id (vector-ref i 1)])
      (make-team id (get-slots-for-team id)))))

(define (add-matches pgc id-saison matches)
  (unless (zero? (length matches))
    (query-exec pgc (~a "DELETE FROM rush_4v4_matchs WHERE idSaison = " id-saison))
    (query-exec pgc (~a "INSERT INTO rush_4v4_matchs "
                        "(idSaison, idSlot, idTeam1, idTeam2) "
                        "VALUES "
                        (string-join (map (λ (i)
                                            (affectation-to-string id-saison i)) matches) ", ")))))

;; (define (insert-teams pgc id-saison teams)
;;   (query-exec pgc "TRUNCATE rush_4v4_registedteams")
;;   (query-exec pgc (~a "INSERT INTO rush_4v4_registedteams "
;;                       "(idSaison, idTeam) "
;;                       "VALUES "
;;                       (string-join (map (λ (i)
;;                                           (~a "(" id-saison ", " (team-id i) ")")) teams) ", "))))

;; (define (insert-slots pgc teams)
;;   (query-exec pgc "TRUNCATE rush_4v4_timeslots_selected")
;;   (query-exec pgc (~a "INSERT INTO rush_4v4_timeslots_selected "
;;                       "(idTeam, idSlot) "
;;                       "VALUES "
;;                       (string-join (foldr (λ (i l)
;;                                             (append (map (λ (j)
;;                                                            (~a "(" (team-id i) ", " j ")")) (team-slots i)) l)) '() teams) ", ")))
;;   )
