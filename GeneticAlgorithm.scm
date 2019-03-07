#lang racket

(require racket/include)
;; Funcion que recibe el gen de un individuo  y calcula el fitness de este
;; El gen es una matriz con el primer elemento es la velocidad,  el segundo  es la fuerza, y el tercero es la habilidad
(define (fitnessDelantero genIndividuo)
    (list (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo) (cadddr genIndividuo) (fitnessDelantero-aux genIndividuo)))
(define (fitnessDelantero-aux genIndividuo)
    (+ (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo)))
(define (fitnessPortero genPortero)
    (list (car genPortero) (cadr genPortero) (caddr genPortero) (cadddr genPortero) (fitnessPortero-aux genPortero)))
(define (fitnessPortero-aux genPortero)
    (+ (car genPortero) (cadr genPortero) (caddr genPortero)))

(define (fitnessMedioCampista genMedioCampista)
    (list (car genMedioCampista) (cadr genMedioCampista) (caddr genMedioCampista) (cadddr genMedioCampista) (fitnessMedioCampista-aux genMedioCampista)))
(define (fitnessMedioCampista-aux genMedioCampista)
    (+ (car genMedioCampista) (cadr genMedioCampista) (caddr genMedioCampista)))



(define (player)
    (list (+ (random 10) 1) (+ (random 10) 1) (+ (random 10) 1)))

(define (portero)
    (list (append (player) (list(list 10 10)) '(0))))

(define (defensa cantDefensas)
    (cond 
        ((zero? cantDefensas) 
            '())
        (else 
            (cons   (append (player) (list (list 10 10)) '(0)) (defensa (- cantDefensas 1)))))) 
(define (volante cantVolante)
    (cond 
        ((zero? cantVolante) 
            '())
        (else 
            (cons   (append (player) (list (list 10 10)) '(0)) (volante (- cantVolante 1)))))) 
(define (delantero cantDelantero)
    (cond 
        ((zero? cantDelantero) 
            '())
        (else 
            (cons   (append (player) (list (list 10 10))'(0)) (delantero (- cantDelantero 1)))))) 

(define (firstGen alignment)
    (append (list(portero)) (list(defensa (car alignment))) (list(volante (cadr alignment)))  (list(delantero (caddr alignment)))))

(define (nextGoalkeepers ParentsA ParentsB)
    (append (nextGoalkeepers-aux ParentsA ParentsB '()) (nextGoalkeepers-aux ParentsB ParentsA '())))

(define (nextGoalkeepers-aux ParentA ParentB offspring)
    (cond 
        ((null? ParentA) 
            offspring)
        (else 
            (nextGoalkeepers-aux (cdr ParentA) (cdr ParentB) (append offspring (crossoverGoalkeeper (car ParentA) (car ParentB)))))))
(define (crossoverGoalkeeper ParentA ParentB)
    (list (car ParentB) (cadr ParentA) (caddr ParentB) (cdddr ParentA)))

(define (nextMidFielder TeamA TeamB)
    (append  (crossoverMidField (caddr TeamB) (caddr TeamA) '()) 
            (crossoverMidField (caddr TeamA) (caddr TeamB) '())))

(define (crossoverMidField MidFieldersA MidFieldersB offspringMidFielders)
    (cond (
        (or (null? MidFieldersA) (null? MidFieldersB)) 
            offspringMidFielders)
        (else 
            (crossoverMidField (cdr MidFieldersA) (cdr MidFieldersB) (append offspringMidFielders (crossoverMidField-aux (car MidFieldersA) (car MidFieldersB) ))))))
(define (crossoverMidField-aux ParentA ParentB)
    (list(append (list (car ParentB) (cadr ParentA) (caddr ParentB))  (cdddr ParentA))))




(define (crossover ParentsA ParentsB offspring)
    (cond 
        ( (null? ParentsA )
            offspring)
        (else 
            (crossover (cdr ParentsA) ParentsB (append offspring (crossover-aux (car ParentsA) ParentsB ))))))

(define (crossover-aux ParentA ParentsB)
    (cond 
        ((null? ParentsB) 
            '())
        (else 
            (cons 
                (append (list (caar ParentsB) (cadr ParentA) (caddar ParentsB))  (cdddr ParentA)) 
                (crossover-aux ParentA (cdr ParentsB)) ))))

(define (hijosPortero  EquipoA EquipoB)
    (append (crossover (car EquipoA) (car EquipoB) '()) 
            (crossover (car EquipoB) (car EquipoA) '())))

(define (hijosDefensas EquipoA EquipoB)
    (cond ((null? (cadr EquipoA)) 
            (crossover (cadr EquipoB) (cadr EquipoB) '()))
        ((null? (cadr EquipoB)) 
            (crossover (cadr EquipoA) (cadr EquipoA) '()))
        (else 
            (append (crossover (cadr EquipoA) (cadr EquipoB) '()) 
                    (crossover (cadr EquipoB) (cadr EquipoA) '())))))
(define (hijosMedio EquipoA EquipoB)
    (cond 
        ((null? EquipoA) 
            (crossover (caddr EquipoB) (caddr EquipoB) '()))
        ((null? EquipoB) 
            (crossover (caddr EquipoA) (caddr EquipoA) '()))
        (else
            (append (crossover (caddr EquipoA) (caddr EquipoB) '()) 
                    (crossover (caddr EquipoB) (caddr EquipoA) '())))))

(define (hijosDelantero EquipoA EquipoB)
    (cond 
        ((null? EquipoA) 
            (crossover (cadddr EquipoB) (cadddr EquipoB) '()))
        ((null? EquipoB) 
            (crossover (cadddr EquipoA) (cadddr EquipoA) '()))
        (else
            (append (crossover (cadddr EquipoA) (cadddr EquipoB) '()) 
                    (crossover (cadddr EquipoB) (cadddr EquipoA) '())))))

(define (hijosEquipo EquipoA EquipoB)
    (append (list (fitnessPorteros (hijosPortero EquipoA EquipoB))))) ;(fitnessDefensas (list (hijosDefensas EquipoA EquipoB))) (fitnessMedioCampistas(list(hijosMedio EquipoA EquipoB))) (fitnessDelanteros (list (hijosDelantero EquipoA EquipoB)))))


(define (fitnessPorteros porterosLista)
    (cond 
        ((null? porterosLista) 
            porterosLista)
        (else  
            (cons (fitnessPortero (car porterosLista)) (fitnessPorteros (cdr porterosLista))))))
(define (mutacion ListaJugadoresMutar porcentajeMutacion)
    (cond 
        ((null? ListaJugadoresMutar)
            ListaJugadoresMutar)
        ((equal? (random 1 (+ (quotient 100 porcentajeMutacion) 1)) (random 1 (+ (quotient 100 porcentajeMutacion) 1))) 
            (cons (mutacion-aux (car ListaJugadoresMutar) (random 3)) (mutacion (cdr ListaJugadoresMutar) porcentajeMutacion)))
        (else 
            (cons (car ListaJugadoresMutar) (mutacion (cdr ListaJugadoresMutar) porcentajeMutacion)))))
(define (mutacion-aux individuoAMutar campoMutar)
    (cond 
        ((zero? campoMutar) 
            (cons (random 1 11) (cdr individuoAMutar)))
        (else 
            (cons (car individuoAMutar) (mutacion-aux (cdr individuoAMutar) (- campoMutar 1))))))