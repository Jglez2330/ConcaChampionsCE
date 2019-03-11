#lang racket
(provide firstGen)
(provide siguienteEquipo)

(require racket/include)

;; Funcion que recibe el gen de un individuo  y calcula el fitness de este
;; El gen es una matriz con el primer elemento es la velocidad,  el segundo  es la fuerza, y el tercero es la habilidad
(define (fitnessDelantero genIndividuo)
    (list (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo) (cadddr genIndividuo) (fitnessDelantero-aux genIndividuo)))
;;
;;
(define (fitnessDelantero-aux genIndividuo)
    (+ (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo)))
;;
;;
(define (fitnessPortero genPortero)
    (list (car genPortero) (cadr genPortero) (caddr genPortero) (cadddr genPortero) (fitnessPortero-aux genPortero)))
;;
;;
(define (fitnessPortero-aux genPortero)
    (+ (car genPortero) (cadr genPortero) (caddr genPortero)))
;;
;;
(define (fitnessMedioCampista genMedioCampista)
    (list (car genMedioCampista) (cadr genMedioCampista) (caddr genMedioCampista) (cadddr genMedioCampista) (fitnessMedioCampista-aux genMedioCampista)))
;;
;;
(define (fitnessMedioCampista-aux genMedioCampista)
    (+ (car genMedioCampista) (cadr genMedioCampista) (caddr genMedioCampista)))

;;
;;
(define (fitnessDefensa genDenfensa)
    (list (car genDenfensa) (cadr genDenfensa) (caddr genDenfensa) (cadddr genDenfensa) (fitnessDefensa-aux genDenfensa)))
;;
;;
(define (fitnessDefensa-aux genDenfensa)
    (+ (car genDenfensa) (cadr genDenfensa) (caddr genDenfensa)))

;;
;;
(define (player)
    (list (+ (random 10) 1) (+ (random 10) 1) (+ (random 10) 1)))
;;
;;
(define (portero)
    (list (append (player) (list(list (random 0 1200) (random 0 650))) '(0))))
;;
;;
(define (defensa cantDefensas)
    (cond 
        ((zero? cantDefensas) 
            '())
        (else 
            (cons   (append (player) (list (list(random 0 1200) (random 0 650))) '(0)) (defensa (- cantDefensas 1)))))) 
;;
;;
(define (medioCampista cantMedioCampistas)
    (cond 
        ((zero? cantMedioCampistas) 
            '())
        (else 
            (cons   (append (player) (list (list(random 0 1200) (random 0 650))) '(0)) (medioCampista (- cantMedioCampistas 1)))))) 
(define (delantero cantDelantero)
    (cond 
        ((zero? cantDelantero) 
            '())
        (else 
            (cons   (append (player) (list (list (random 0 1200) (random 0 650)))'(0)) (delantero (- cantDelantero 1)))))) 

(define (firstGen alignment)
    (append (list(portero)) (list(defensa (car alignment))) (list(medioCampista (cadr alignment)))  (list(delantero (caddr alignment)))))

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
    (append (list (fitnessPorteros (mutacion (hijosPortero EquipoA EquipoB) 77))) (list (aptitudDefensas (mutacion (hijosDefensas EquipoA EquipoB) 33))) (list (aptitudMedioCampista (mutacion (hijosMedio EquipoA EquipoB) 33))) (list (aptitudDelanteros (mutacion (hijosDelantero EquipoA EquipoB) 33)))))




(define (aptitudDefensas defensasLista)
    (cond 
        ((null? defensasLista) 
            '())
        (else 
            (cons (fitnessDefensa (car defensasLista)) (aptitudDefensas (cdr defensasLista))))))

(define (aptitudMedioCampista medioCampistasLista)
    (cond 
        ((null? medioCampistasLista) 
            '())
        (else 
            (cons (fitnessMedioCampista (car medioCampistasLista)) (aptitudMedioCampista (cdr medioCampistasLista))))))

(define (aptitudDelanteros delanterosLista)
    (cond 
        ((null? delanterosLista) 
            '())
        (else 
            (cons (fitnessDelantero (car delanterosLista)) (aptitudDelanteros (cdr delanterosLista))))))
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
(define (obtenerProximaGeneracionPorteros  portero proximaGeneracionPorterosLista)
    (cond 
        ((null? proximaGeneracionPorterosLista)
            '())
        (else 
            (car proximaGeneracionPorterosLista))))
(define (obtenerProximaGeneracionDefensas defensas proximaGeneracionDefensasLista)
    (cond 
        ((null? defensas) 
            '())
        ((> (fitnessDefensa-aux (car defensas)) (fitnessDefensa-aux  (car proximaGeneracionDefensasLista)))
            (cons (car defensas) (obtenerProximaGeneracionDefensas (cdr defensas) proximaGeneracionDefensasLista)))
        (else 
            (cons (cambiarPosicion (car proximaGeneracionDefensasLista) (car defensas) fitnessDefensa-aux) (obtenerProximaGeneracionDefensas (cdr defensas) (cdr proximaGeneracionDefensasLista))))))
(define (obtenerProximaGeneracionMedioCampistas medioCampistasLista hijosMedioCampistasLista)
    (cond 
        ((null? medioCampistasLista ) 
            '())
        ((> (fitnessMedioCampista-aux (car medioCampistasLista)) (fitnessMedioCampista-aux (car hijosMedioCampistasLista)))
            (cons (car medioCampistasLista) (obtenerProximaGeneracionDefensas (cdr medioCampistasLista) hijosMedioCampistasLista)))
        (else 
            (cons (cambiarPosicion (car hijosMedioCampistasLista) (car medioCampistasLista) fitnessMedioCampista-aux) (obtenerProximaGeneracionMedioCampistas (cdr medioCampistasLista) (cdr hijosMedioCampistasLista))))))

(define (obtenerProximaGeneracionDelanteros delanterosLista hijosDelanterosLista)
    (cond 
        ((null? delanterosLista) 
            '())
        ((> (fitnessDelantero-aux (car delanterosLista)) (fitnessDelantero-aux (car hijosDelanterosLista)))
            (cons (car delanterosLista) (obtenerProximaGeneracionDelanteros (cdr delanterosLista) hijosDelanterosLista)))
        (else 
            (cons (cambiarPosicion (car hijosDelanterosLista) (car delanterosLista) fitnessDelantero-aux) (obtenerProximaGeneracionDelanteros (cdr delanterosLista) (cdr hijosDelanterosLista))))))
(define (cambiarPosicion hijo padre funcionAptitud)
    (append (list (car hijo) (cadr hijo) (caddr hijo) (cadddr padre) ) (funcionAptitud hijo)))

(define (siguienteEquipo EquipoA EquipoB)
    (list (obtenerProximaGeneracionPorteros (car EquipoA) (quicksort- (hijosPortero EquipoA EquipoB) fitnessPortero-aux)) 
            (obtenerProximaGeneracionDefensas (cadr EquipoA) (quicksort- 
                (hijosDefensas EquipoA EquipoB) fitnessDefensa-aux))
            (obtenerProximaGeneracionMedioCampistas (caddr EquipoA) (quicksort- (hijosMedio EquipoA EquipoB) fitnessMedioCampista-aux))
            (obtenerProximaGeneracionDelanteros (cadddr EquipoA) (quicksort- (hijosDelantero EquipoA EquipoB) fitnessDelantero-aux))))
(define (quicksort- hijos funcionAptitud)
    (cond ((null? hijos) 
            '())
    (else
        (quicksort-aux (car hijos) (cdr hijos) funcionAptitud '() '()))))

(define (quicksort-aux pivot lista funcionAptitud menores mayores)
    (cond 
        ((null? lista)(append (quicksort- menores funcionAptitud) (list pivot) (quicksort- mayores funcionAptitud)))
        ((< (funcionAptitud pivot) (funcionAptitud (car lista))) (quicksort-aux pivot (cdr lista) funcionAptitud menores (cons (car lista) mayores)))
        (else (quicksort-aux pivot (cdr lista) funcionAptitud (cons (car lista) menores) mayores))))
