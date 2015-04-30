#lang racket

;; id => id de la team
;; slots => list des créneaux disponibles
(struct team (id slots used-slots) #:transparent)
(struct affectation (slot team1 team2) #:transparent)

(define match-number 4)
(define all-slots '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10))

;; crée une team
;; id => id de la team
;; slots => slots disponibles
;; used-slots => slots deja utilisés [facultatif]
(define (make-team id slots [used-slots '()])
  (team id slots used-slots))

(define (make-affectation slot team1 team2)
  (affectation slot (team-id team1) (team-id team2)))

(define (team-equal? t1 t2)
  (equal? (team-id t1) (team-id t2)))

;; match-number => nombre de matchs désiré
;; team => une team
;; vrai quand nombre match < nombre assignations
(define (available? match-number team)
  (and (< (length (team-used-slots team)) match-number)))

;;ordonne les slots par nombre de teams ayant le slot
;;le slot est en tête de list suivi des teams ayant le slot
(define (sort-slots match-number teams)
;;crée une list de team pour chaque slot
  (define (make-teams-hash teams)
    (let ([h (make-hash)])
      (for-each (λ (i)
                  (when (available? match-number i)
                    (for-each (λ (j)
                                (hash-set! h j (cons i (hash-ref h j '()))))
                              (team-slots i)))) teams)
      h))
  (map (λ (i)
         (cons (car i)
               (sort (cdr i)
                     (λ (i j)
                       (< (length (team-slots i)) (length (team-slots j)))))))
       (sort (hash->list (make-teams-hash teams)) (λ (i j)
                                                    (> (length i) (length j))))))

;; marque le slot comme utilisé et l'enleve des slots disponibles
(define (use-slot team slot)
  (make-team (team-id team)
             (remove slot (team-slots team))
             (cons slot (team-used-slots team))))

;; match-number => nombre de match
;; sorted-slots => slots triés par nombre de teams
;; crée tous les matches possibles pour le slot le plus peuplé
(define (make-matches match-number teams)
  (let* ([sorted-slots (sort-slots match-number teams)]
         [slot (caar sorted-slots)]
         [t (filter (λ (i)
                      (available? match-number i)) (cdar sorted-slots))])
    (let f ([t t] [used '()] [affectations '()])
      (if (< (length t) 2)
          (let ([teams (filter (λ (i)
                                  (not (memf (λ (j)
                                               (team-equal? i j)) used))) teams)])
            `(,(append teams used) ,affectations))
          (let* ([t1 (use-slot (car t) slot)]
                 [t2 (use-slot (cadr t) slot)]
                 [t (remove (car t) (remove (cadr t) t))])
            (f t
               (append used `(,t1) `(,t2))
               (append affectations `(,(make-affectation slot t1 t2)))))))))

(define (make-n-random-teams slots n)
  (define (select-random-slots slots nbslots)
    (take (shuffle slots) (min (- (length slots) 1) nbslots))) 
  (build-list n (λ (i)
                  (make-team i (select-random-slots slots (random (length slots)))))))

;; (time (sort-slots match-number t)(void))
(define (test)
  (define t (shuffle (make-n-random-teams all-slots 10)))
  (let f ([teams t] (affectations '()))
    (let ([res (make-matches match-number teams)])
      (if (empty? (cadr res))
          (begin
            (pretty-display teams)
            affectations)
          (f (car res) (append affectations (cadr res)))))))
