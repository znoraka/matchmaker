#lang racket
(provide make-team)

;; id => id de la team
;; slots => list des créneaux disponibles
(struct team (id slots used-slots opponents) #:transparent)
(struct affectation (slot team1 team2) #:transparent)

(define (generate-slots n)
  (build-list n values))

;; crée une team
;; id => id de la team
;; slots => slots disponibles
;; used-slots => slots deja utilisés [facultatif]
(define (make-team id slots . l)
  (let ([l (append l '(() () ()))])
    (team id slots (car l) (cadr l))))

(define (make-affectation slot team1 team2)
  (affectation slot (team-id team1) (team-id team2)))

(define (team-equal? t1 t2)
  (equal? (team-id t1) (team-id t2)))

;; match-number => nombre de matchs désiré
;; team => une team
;; vrai quand nombre match < nombre assignations
(define (available? match-number team)
  (and (< (length (team-used-slots team)) match-number)))

;;renvoi les teams disponibles
(define (available-teams match-number teams)
  (filter available? teams))

;;ordonne les slots par nombre de teams ayant le slot
;;le slot est en tête de list suivi des teams ayant le slot
(define (sort-slots match-number teams all-slots)
  ;;crée une list de team pour chaque slot
  (define (make-teams-hash teams)
    (let ([h (make-hash)])
      (for-each (λ (i)
                  (when (available? match-number i)
                    (for-each (λ (j)
                                (hash-set! h j (cons i (hash-ref h j '()))))
                              (team-slots i)))) teams)
      h))
  (sort (hash->list (make-teams-hash teams)) (λ (i j)
                                               (> (length i) (length j)))))

;;ajoute tous les slots non utilisés aux teams sans slot
(define (prepare-teams teams all-slots)
  (define (make-all-unused-slots used-slots)
    (filter (λ (i)
              (not (member i used-slots))) all-slots))
  
  (map (λ (i)
         (if (empty? (team-slots i))
             (make-team (team-id i)
                        (make-all-unused-slots (team-used-slots i))
                        (team-used-slots i)
                        (team-opponents i))
             i)) teams))

;; marque le slot comme utilisé et l'enleve des slots disponibles
(define (use-slot team slot all-slots opponent)
  (let ([slots (remove slot (team-slots team))])
      (make-team (team-id team)
                 slots
                 (cons slot (team-used-slots team))
                 (cons (team-id opponent) (team-opponents team)))))

;; match-number => nombre de match
;; sorted-slots => slots triés par nombre de teams
;; crée tous les matches possibles pour le slot le plus peuplé
(define (make-matches match-number teams all-slots)
  (define (already-matched? t1 t2)
    (member (team-id t2) (team-opponents t1)))

  ;;les teams ont au moins un slot en commun
  (define (find-match teams)
    (for*/or ([t1 teams]
              [t2 teams])
      (if (or (team-equal? t1 t2) (already-matched? t1 t2))
          #f
            `(,t1 ,t2))))

  (define (format-to-return teams used affectations)
    (let ([teams (filter (λ (i)
                           (not (memf (λ (j)
                                        (team-equal? i j)) used))) teams)])
      `(,(append teams used) ,affectations)))

  (let* ([sorted-slots (sort-slots match-number (prepare-teams teams all-slots) all-slots)])
    (if (empty? sorted-slots)
        '(() ())
        (let ([slot (caar sorted-slots)]
              [t (filter (λ (i)
                           (available? match-number i)) (cdar sorted-slots))])
          (let f ([t t] [used '()] [affectations '()])
            (if (< (length t) 2)
                (format-to-return teams used affectations)
                (let ([match-found (find-match (shuffle t))])
                  (if match-found
                      (let* ([temp1 (car match-found)]
                             [temp2 (cadr match-found)]
                             [t1 (use-slot temp1 slot all-slots temp2)]
                             [t2 (use-slot temp2 slot all-slots temp1)]
                             [t (remove temp1 (remove temp2 t))])
                        (f t
                           (append used `(,t1) `(,t2))
                           (append affectations `(,(make-affectation slot t1 t2)))))
                      (format-to-return teams used affectations)))))))))

(define (make-n-random-teams slots n)
  (define (select-random-slots slots nbslots)
    (take (shuffle slots) (min (- (length slots) 1) nbslots))) 
  (build-list n (λ (i)
                  (make-team i (select-random-slots slots
                                                    (random 10)
                                                    ;; (+ 1 (random (- (length slots) 1)))
                                                    )))))

(define (test t all-slots match-number)
  (let f ([teams t] (affectations '()))
    (let ([res (make-matches match-number teams all-slots)])
      (if (empty? (cadr res))
          (begin
            ;; (pretty-display teams)
           ;; (pretty-display affectations)
            ;; (displayln (~a (length affectations) '/ (/ (* match-number n) 2)))
           `(,(length affectations) ,affectations ,teams))
          (f (car res) (append affectations (cadr res)))))))

(define (solve slots-number teams-number match-number tries)
  (define all-slots (generate-slots slots-number))
  (define t (shuffle (make-n-random-teams all-slots teams-number)))
  (define required-matchs-number (truncate (/ (* match-number teams-number) 2)))
  
  (define (format-to-return res)
    (list (cadr res) `(,(car res) / ,required-matchs-number)))
  
  ;; (define t (list (team 8 '(8 0 3 1 5) '() '())
  ;;                 (team 1 '(2 5 0) '() '())
  ;;                 (team 14 '(1 8 3 2 9 7 0 6) '() '())
  ;;                 (team 17 '(1 2 3 0 9 8) '() '())
  ;;                 (team 10 '(1 4 6 7 3 2) '() '())
  ;;                 (team 13 '(1 0 2 8 7 9 3) '() '())
  ;;                 (team 16 '(2 8 6 7 4 3 9 0 5) '() '())
  ;;                 (team 0 '(0 6 1 3 2 8 5 7 4) '() '())
  ;;                 (team 7 '(0 2 1 3 6 5 7 9) '() '())
  ;;                 (team 6 '(7) '() '())
  ;;                 (team 18 '() '() '())
  ;;                 (team 19 '(8 9 6 1 0 7 5 3 4) '() '())
  ;;                 (team 2 '() '() '())
  ;;                 (team 9 '(4 7 9) '() '())
  ;;                 (team 11 '(6 7 0 3) '() '())
  ;;                 (team 3 '(1 6 3 8) '() '())
  ;;                 (team 4 '(8 7 5 4 2 0 6 1) '() '())
  ;;                 (team 15 '(3 0 5 9) '() '())
  ;;                 (team 5 '(1) '() '())
  ;;                 (team 12 '() '() '())))

  ;; (pretty-display t)
  (displayln (~a "matchs attendus : " required-matchs-number))

  (let f ([tries tries]
          [best '(0 0)])
    (if (= tries 0)
        (format-to-return best)
        (let ([res (test t all-slots match-number)])
          (if (= (car res) required-matchs-number)
              (format-to-return res)
              (f (- tries 1)
                 (if (> (car res) (car best))
                     res
                     best)))))))

(define (check-result affectations)
  (for*/or ([i (caddr affectations)]
         [j (caddr affectations)])
    (if (and (not (eq? i j))
             (let ([a (min (affectation-team1 i) (affectation-team2 i))]
                   [b (max (affectation-team1 i) (affectation-team2 i))]
                   [c (min (affectation-team1 j) (affectation-team2 j))]
                   [d (max (affectation-team1 j) (affectation-team2 j))])
               (and (= a c) (= b d))))
        (error (~a i '- j))
        #f)))
