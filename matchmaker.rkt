#lang racket

(require racket/format)

(provide make-team)
(provide affectation-to-string)
(provide solve)
(provide generate-slots)
(provide make-n-random-teams)
(provide team-id)
(provide team-slots)

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

(define (affectation-to-string id-saison a)
  (~a "("
      id-saison ", "
      (affectation-slot a) ", "
      (affectation-team1 a) ", "
      (affectation-team2 a) ")"))

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
(define (sort-slots match-number teams all-slots max-matches-per-slot slots-use-count)
  ;;crée une list de team pour chaque slot
  (define (make-teams-hash teams)
    (let ([h (make-hash)])
      (for-each (λ (i)
                  (when (available? match-number i)
                    (for-each (λ (j)
                                (hash-set! h j (cons i (hash-ref h j '()))))
                              (team-slots i)))) teams)
      h))
  (sort (filter (λ (i)
                  (not (or (< (length i) 3) (> (hash-ref slots-use-count (car i)) max-matches-per-slot))))
                (hash->list (make-teams-hash teams)))
        (λ (i j)
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
(define (make-matches match-number teams all-slots max-matches-per-slot used-slots-count)
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

  (let* ([sorted-slots (sort-slots match-number (prepare-teams teams all-slots) all-slots max-matches-per-slot used-slots-count)])
    (if (empty? sorted-slots)
        '(() ())
        (let ([slot (caar sorted-slots)]
              [t (filter (λ (i)
                           (available? match-number i)) (cdar sorted-slots))])
          (let f ([t t] [used '()] [affectations '()])
            (if (or (< (length t) 2) (>= (hash-ref used-slots-count slot) max-matches-per-slot))
                (format-to-return teams used affectations)
                (let ([match-found (find-match (shuffle t))])
                  (if match-found
                      (let* ([temp1 (car match-found)]
                             [temp2 (cadr match-found)]
                             [t1 (use-slot temp1 slot all-slots temp2)]
                             [t2 (use-slot temp2 slot all-slots temp1)]
                             [t (remove temp1 (remove temp2 t))])
                        (hash-set! used-slots-count slot (+ (hash-ref used-slots-count slot) 1))
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

(define (test t all-slots match-number max-matches-per-slot)
  (define used-slots-count (make-hash))
  (for-each (λ (i)
              (hash-set! used-slots-count i 0)) all-slots)

  (let f ([teams t] (affectations '()))
    (let ([res (make-matches match-number teams all-slots max-matches-per-slot used-slots-count)])
      (if (empty? (cadr res))
           `(,(length affectations) ,affectations ,teams)
          (f (car res) (append affectations (cadr res)))))))

;; (define all-slots (generate-slots slots-number))
;; (define t (shuffle (make-n-random-teams all-slots teams-number)))

(define (solve teams all-slots match-number tries max-matches-per-slot)
  (define required-matchs-number (truncate (/ (* match-number (length teams)) 2)))
  
  (define (format-to-return res t)
    (list
     (cadr res) `(,(car res) / ,required-matchs-number matches in ,(- tries t) tries)
     (map (λ (i)
            `(team ,(team-id i) only has ,(length (team-used-slots i)) matches))
          (filter (λ (i)
                    (available? match-number i)) (caddr res)))))
  
  (let f ([tries tries]
          [best '(0 0)])
    (if (= tries 0)
        (format-to-return best tries)
        (let ([res (test teams all-slots match-number max-matches-per-slot)])
          (if (= (car res) required-matchs-number)
              (format-to-return res tries)
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

 
