#lang racket

;; id => id de la team
;; slots => list des créneaux disponibles
(struct team (id slots used-slots opponents) #:transparent)
(struct affectation (slot team1 team2) #:transparent)

(define match-number 5)
;(define all-slots '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20))
;(define all-slots '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10))

(define (generate-slots n)
  (build-list n (λ (i)
                  (~a 's i))))

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
(define (use-slot team slot all-slots opponent)
  (define (make-all-unused-slots used-slots all-slots)
    (filter (λ (i)
              (not (member i used-slots))) all-slots))
  
  (let ([slots (remove slot (team-slots team))])
    (let ([slots (if (empty? slots) (make-all-unused-slots (team-used-slots team) all-slots) slots)])
      (make-team (team-id team)
                 slots
                 (cons slot (team-used-slots team))
                 (cons (team-id opponent) (team-opponents team))))))

;; match-number
;;=> nombre de match
;; sorted-slots => slots triés par nombre de teams
;; crée tous les matches possibles pour le slot le plus peuplé
(define (make-matches match-number teams all-slots)
  (define (already-matched? t1 t2)
    (member (team-id t2) (team-opponents t1)))
  
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

  (let* ([sorted-slots (sort-slots match-number teams)]
         [slot (caar sorted-slots)]
         [t (filter (λ (i)
                      (available? match-number i)) (cdar sorted-slots))])
    (let f ([t t] [used '()] [affectations '()])
      (if (< (length t) 2)
         (format-to-return teams used affectations)
         (let ([match-found (find-match t)])
           (if match-found
               (let* ([temp1 (car match-found)]
                      [temp2 (cadr match-found)]
                      [t1 (use-slot temp1 slot all-slots temp2)]
                      [t2 (use-slot temp2 slot all-slots temp1)]
                      [t (remove temp1 (remove temp2 t))])
                 (f t
                    (append used `(,t1) `(,t2))
                    (append affectations `(,(make-affectation slot t1 t2)))))
               (format-to-return teams used affectations)))))))

(define (make-n-random-teams slots n)
  (define (select-random-slots slots nbslots)
    (take (shuffle slots) (min (- (length slots) 1) nbslots))) 
  (build-list n (λ (i)
                  (make-team i (select-random-slots slots (+ 1 (random (- (length slots) 1))))))))

(define (test n s)
  (define all-slots (generate-slots s))
  (define t (shuffle (make-n-random-teams all-slots n)))
  (let f ([teams t] (affectations '()))
    (let ([res (make-matches match-number teams all-slots)])
      (if (empty? (cadr res))
          (begin
           (pretty-display teams)
           (pretty-display affectations)
           (displayln (~a (length affectations) '/ (/ (* match-number n) 2)))
           (for* ([i affectations]
                  [j affectations])
             (when (and (not (eq? i j))
                        (let ([a (min (affectation-team1 i) (affectation-team2 i))]
                              [b (max (affectation-team1 i) (affectation-team2 i))]
                              [c (min (affectation-team1 j) (affectation-team2 j))]
                              [d (max (affectation-team1 j) (affectation-team2 j))])
                          (and (= a c) (= b d))))

                        (displayln (~a i '- j)))
             ))
          (f (car res) (append affectations (cadr res)))))))

