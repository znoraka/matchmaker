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
  (sort (hash->list (make-teams-hash teams)) (λ (i j)
                                              (> (length i) (length j)))))

;; ;; week-number => nombre de semaines pour le tournoi
;; ;; teams => list de struct team
;; ;; composition entre les slots et le numero des semaines
;; (define (create-slots slots teams)
;;   (map (λ (i)
;;          (make-team (team-id i)
;;                (foldr (λ (n l)
;;                         (append
;;                          (map (λ (slot) (~a 's n '- slot)) (team-slots i))
;;                          l)) '() (build-list week-number values)))) teams))

;; ;; lance une erreur si le nombre de slots est inférieur
;; ;; au nombre de matchs
;; (define (check-teams match-number teams)
;;   (for-each (λ (i)
;;               (when (> match-number (length (team-slots i)))
;;                 (error (~a "erreur : la team " (team-id i) " n'a pas assez d'assignations\n"
;;                            "nombre de matchs : " match-number " - nombre d'assignations : "
;;                            (length (team-slots i)))))) teams))

;; teams => list de struct de team
;; list des affectations
 (define (compute-affectations teams)
   (void))

;; teams => list de struct de team
;; trouve deux teams avec une affectation possible
(define (select-teams match-number teams)
  (define (make-team-pair t1 t2)
    (if (or (not (or (available? match-number t1) (available? match-number t2))) (eq? t1 t2))
        #f
        (let ([x (for*/or ([i (team-slots t1)]
                           [j (team-slots t2)])
                   ((λ (i j)
                      (if (equal? i j)
                          i
                          #f)) i j))])
          (if x `(,t1 ,t2 ,x) #f))))
  (for*/or ([i teams]
            [j teams])
    (make-team-pair i j)))

(define (make-n-random-teams slots n)
  (define (select-random-slots slots nbslots)
    (take (shuffle slots) (min (- (length slots) 1) nbslots)))
    
  (build-list n (λ (i)
                  (make-team i (select-random-slots slots (random 10))))))

(define t (shuffle (make-n-random-teams all-slots 10)))
(time (sort-slots match-number t))
