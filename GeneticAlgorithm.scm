#lang racket
(provide firstGen)
(provide siguienteEquipo)
(provide primeraGeneracionEquipoIzquierda)
(provide primeraGeneracionEquipoDerecha)


(require racket/include)

;; Funcion que recibe el gen de un individuo  y calcula el fitness de este
;; El gen es una matriz con el primer elemento es la velocidad,  el segundo  es la fuerza, y el tercero es la habilidad

(define (fitnessDelantero genIndividuo)
    (list (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo) (cadddr genIndividuo) (fitnessDelantero-aux genIndividuo)))


;;Funcion que recibe el gen de un delantero y calcula el valor numerico de la aptitud
;;E: lista que representa el jugador (Gen individuo)
;;S: vlaor numerico de la aptitud

(define (fitnessDelantero-aux genIndividuo)
    (+ (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo)))


;;Funcion que sirve de punto de entrada para calcular la aptitud del portero
;;E: Una lista que representa al portero
;;S: El jugador con el valor de la aptitud ya calculado

(define (fitnessPortero genPortero)
    (list (car genPortero) (cadr genPortero) (caddr genPortero) (cadddr genPortero) (fitnessPortero-aux genPortero)))


;;Funcion que calcula el valor numerico de la aptitud del portero
;;E: Se ingresa una lista que represanta al jugador
;;S: Representavcion numeroca de la aptitud del portero 

(define (fitnessPortero-aux genPortero)
    (+ (car genPortero) (cadr genPortero) (caddr genPortero)))


;;Funcion que calcula la representacion numerica de la aptitud del medio campista y la agrega a la lista
;;E: Una lista que representa al jugador 
;;S: Una lista que representa un jugador con la aptitud calculada en la ultima posicion

(define (fitnessMedioCampista genMedioCampista)
    (list (car genMedioCampista) 
            (cadr genMedioCampista) 
            (caddr genMedioCampista) 
            (cadddr genMedioCampista) 
            (fitnessMedioCampista-aux genMedioCampista)))


;;Funcion que calcula el volar numerico de la representacion de la aptitud del jugador 
;;E: Lita que representa al jugador 
;;S: Valor numerico que representa la aptitud del jugador

(define (fitnessMedioCampista-aux genMedioCampista)
    (+ (car genMedioCampista) (cadr genMedioCampista) (caddr genMedioCampista)))


;;Funcion que calcula la respresentacion de la aptitud del jugador y agrega la aptitud al final
;;E: Lista que representa al jugador Defensa 
;;S: Lista del jugador Defensa con la aptitud ya calculada

(define (fitnessDefensa genDenfensa)
    (list (car genDenfensa) (cadr genDenfensa) (caddr genDenfensa) (cadddr genDenfensa) (fitnessDefensa-aux genDenfensa)))


;;Funcion que calcula el valor numerico de la aptitud de la defensa
;;E: Lista que representa al jugador
;;S: Valor numerico que representa la aptitud del jugador Defensa

(define (fitnessDefensa-aux genDenfensa)
    (+ (car genDenfensa) (cadr genDenfensa) (caddr genDenfensa)))



;;Funcion que calcula la aptitud de una lista de defensas
;;E:Lista con los jugadores defensas
;;S: Lista de los jugadores defensas con sus respectivas aptitudes calculadas

(define (aptitudDefensas defensasLista)
    (cond 
        ((null? defensasLista) 
            '())
        (else 
            (cons (fitnessDefensa (car defensasLista)) (aptitudDefensas (cdr defensasLista))))))


;;Funcion que calcula todos los medio campistas que se encuentren en una lista
;;E: Lista con medio campistas
;;S: Lista de medio campistas con la aptitud calculada

(define (aptitudMedioCampista medioCampistasLista)
    (cond 
        ((null? medioCampistasLista) 
            '())
        (else 
            (cons (fitnessMedioCampista (car medioCampistasLista)) (aptitudMedioCampista (cdr medioCampistasLista))))))


;;Funcion que calcula la aptitud de los delanteros en una lista
;;E: Lsita de delanteros
;;S: Lista de delanteros con las aptitudes calculadas

(define (aptitudDelanteros delanterosLista)
    (cond 
        ((null? delanterosLista) 
            '())
        (else 
            (cons (fitnessDelantero (car delanterosLista)) (aptitudDelanteros (cdr delanterosLista))))))

 ;;Funcion que calcula la aptitud de los porteros de un lista
 ;;E: lista que contiene porteros
 ;;S: lista de porteros con la aptitud calculada

(define (fitnessPorteros porterosLista)
    (cond 
        ((null? porterosLista) 
            porterosLista)
        (else  
            (cons (fitnessPortero (car porterosLista)) (fitnessPorteros (cdr porterosLista))))))

;;-------------------------------------------------------- Fin funciones de fitness ---------------------------------------------------------


;;Funcion que calcula un jugador base para cualquier posicion en el campo de juego
;;S: lista con los genes de velocidad, fuerza y habilidas respectivos

(define (jugadorBase)
    (list (+ (random 10) 1) (+ (random 10 20) 1) (+ (random 10) 1)))


;;Funcion que genera un portero 
;;E: Posicion en el eje horizontal
;;S: lista que representa un portero

(define (portero posicionX)
    (list (append (jugadorBase) (list(list posicionX 325) 0))))


;;Funcion que genera una lista de defensas
;; E: cantidad de defensas, posicion en el eje horizontal, cantidad total de defensas
;;S: lista de defensas con sus respectivas posiciones

(define (defensa cantDefensas posicionX cantDefensasOriginal)
    (cond 
        ((zero? cantDefensas) 
            '())
        (else 
            (cons   (append (jugadorBase) (list (list posicionX (definirPosicionY cantDefensasOriginal cantDefensas)) 0 )) 
                    (defensa (- cantDefensas 1) posicionX cantDefensasOriginal)))))


;;Funcion que genera una lista de medio campistas
;;E: cantidad de medio campistas, posicion horizontal, cantidad total de medio campistas
;;S: lista con los medio campistas con sus respectivas posiciones

(define (medioCampista cantMedioCampistas posicionX cantMedioCampistasOriginal)
    (cond 
        ((zero? cantMedioCampistas) 
            '())
        (else 
            (cons   (append (jugadorBase) 
                    (list (list (+ posicionX (random -75 25)) (+ (definirPosicionY cantMedioCampistasOriginal cantMedioCampistas) (random -50  25))) 0)) 
                    (medioCampista (- cantMedioCampistas 1) posicionX cantMedioCampistasOriginal))))) 

;;Funcion que genera una lista de delanteros
;; E: cantidad de delanteros, posicion horizontal, cantidad total de delanteros
;; S: lista de delanteros con sus respectivas posiciones

(define (delantero cantDelantero posicionX cantDelanteroOriginal)
    (cond 
        ((zero? cantDelantero) 
            '())
        (else 
            (cons   (append (jugadorBase) (list (list posicionX (definirPosicionY cantDelanteroOriginal cantDelantero)) 0)) 
                    (delantero (- cantDelantero 1) posicionX cantDelanteroOriginal))))) 


;;Funcion que genera el equipo completo 
;;E: aliniacion del equipo, posicion en el campo con respecto al centro
;;S: lista con listas de jugadores

(define (firstGen alignment posicionX)
    (append (list (portero (abs (- posicionX 550)))) 
            (list(defensa (car alignment) (abs (- posicionX 415)) (car alignment))) 
            (list(medioCampista (cadr alignment) (abs (- posicionX 275)) (cadr alignment)))  
            (list(delantero (caddr alignment) (abs (- posicionX 140)) (caddr alignment)))))


;;Funcion que genera el equipo rojo
;;E: formacion del equipo rojo , una lista
;;S: Equipo con sus respectivas posiciones al lado izquierdo de la cancha

(define (primeraGeneracionEquipoIzquierda formacion)
    (firstGen formacion 600))


;;Funcion que genera el equipo azul
;;E: formacion del equipo azul
;;S: Equipo azul con sus respectiva posiciones en el lado derecho de la cancha

(define (primeraGeneracionEquipoDerecha formacion)
    (firstGen formacion -600))


;;Funcion que otorga la posicion en el eje vertical
;;E: cantidad de jugadores total, cantidad de juagadores  
;;S: Vlaor numerico de la posicion en el eje Y

(define (definirPosicionY cantJugadoresOriginal cantJugadores )
    (+ (abs (- (* (quotient 650 cantJugadoresOriginal) cantJugadores) 650)) (quotient (quotient 650 cantJugadoresOriginal) 3)))
;;----------------------------------------------- Fin de la funciones de la primera generacion -------------------------------------------

;;
;;
;;
(define (crossover ParentsA ParentsB offspring funcionAptitud)
    (cond 
        ( (null? ParentsA )
            offspring)
        (else 
            (crossover (cdr ParentsA) ParentsB (append offspring (crossover-aux (car ParentsA) ParentsB funcionAptitud)) funcionAptitud))))

(define (crossover-aux ParentA ParentsB funcionAptitud)
    (cond 
        ((null? ParentsB) 
            '())
        (else 
            (cons 
             (append  (list (caar ParentsB) (cadr ParentA) (caddar ParentsB)) (cdddr ParentA))
                (crossover-aux ParentA (cdr ParentsB) funcionAptitud) ))))

(define (hijosPortero  EquipoA EquipoB)
    (append (crossover (car EquipoA) (car EquipoB) '() fitnessPortero) 
           (crossover (car EquipoB) (car EquipoA) '() fitnessPortero)
           (crossover (car EquipoB) (car EquipoB) '() fitnessPortero)
           (crossover (car EquipoA) (car EquipoA) '() fitnessPortero)))

(define (hijosDefensas EquipoA EquipoB)
    (cond ((null? (cadr EquipoA)) 
            (crossover (cadr EquipoB) (cadr EquipoB) '() fitnessDefensa))
        ((null? (cadr EquipoB)) 
            (crossover (cadr EquipoA) (cadr EquipoA) '() fitnessDefensa))
        (else 
            (append (crossover (cadr EquipoA) (cadr EquipoB) '() fitnessDefensa) 
                    (crossover (cadr EquipoB) (cadr EquipoA) '() fitnessDefensa)
                    (crossover (cadr EquipoB) (cadr EquipoB) '() fitnessDefensa)
                    (crossover (cadr EquipoA) (cadr EquipoA) '() fitnessDefensa)))))
(define (hijosMedio EquipoA EquipoB)
    (cond 
        ((null? EquipoA) 
            (crossover (caddr EquipoB) (caddr EquipoB) '() fitnessMedioCampista))
        ((null? EquipoB) 
            (crossover (caddr EquipoA) (caddr EquipoA) '() fitnessMedioCampista))
        (else
            (append (crossover (caddr EquipoA) (caddr EquipoB) '() fitnessMedioCampista) 
                    (crossover (caddr EquipoB) (caddr EquipoA) '() fitnessMedioCampista)
                    (crossover (caddr EquipoB) (caddr EquipoB) '() fitnessMedioCampista)
                    (crossover (caddr EquipoA) (caddr EquipoA) '() fitnessMedioCampista)))))

(define (hijosDelantero EquipoA EquipoB)
    (cond 
        ((null? EquipoA) 
            (crossover (cadddr EquipoB) (cadddr EquipoB) '() fitnessDelantero))
        ((null? EquipoB) 
            (crossover (cadddr EquipoA) (cadddr EquipoA) '() fitnessDelantero))
        (else
            (append (crossover (cadddr EquipoA) (cadddr EquipoB) '() fitnessDelantero) 
                    (crossover (cadddr EquipoB) (cadddr EquipoA) '() fitnessDelantero)
                    (crossover (cadddr EquipoA) (cadddr EquipoA) '() fitnessDelantero)
                    (crossover (cadddr EquipoB) (cadddr EquipoB) '() fitnessDelantero)))))

(define (hijosEquipo EquipoA EquipoB)
    (append (list (fitnessPorteros (mutacion (hijosPortero EquipoA EquipoB) 77))) 
            (list (aptitudDefensas (mutacion (hijosDefensas EquipoA EquipoB) 33))) 
            (list (aptitudMedioCampista (mutacion (hijosMedio EquipoA EquipoB) 33))) 
            (list (aptitudDelanteros (mutacion (hijosDelantero EquipoA EquipoB) 33)))))




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
            (list(car proximaGeneracionPorterosLista)))))
(define (obtenerProximaGeneracionDefensas defensas proximaGeneracionDefensasLista)
    (cond 
        ((null? defensas) 
            '())
        ((> (fitnessDefensa-aux (car defensas)) (fitnessDefensa-aux  (car proximaGeneracionDefensasLista)))
            (cons (fitnessDefensa(car defensas)) 
                    (obtenerProximaGeneracionDefensas (cdr defensas) proximaGeneracionDefensasLista)))
        (else 
            (cons (cambiarPosicion (car proximaGeneracionDefensasLista) (car defensas) fitnessDefensa) 
                    (obtenerProximaGeneracionDefensas (cdr defensas) (cdr proximaGeneracionDefensasLista))))))
(define (obtenerProximaGeneracionMedioCampistas medioCampistasLista hijosMedioCampistasLista)
    (cond 
        ((null? medioCampistasLista ) 
            '())
        ((> (fitnessMedioCampista-aux (car medioCampistasLista)) (fitnessMedioCampista-aux (car hijosMedioCampistasLista)))
            (cons (fitnessMedioCampista (car medioCampistasLista)) 
                    (obtenerProximaGeneracionDefensas (cdr medioCampistasLista) hijosMedioCampistasLista)))
        (else 
            (cons (cambiarPosicion (car hijosMedioCampistasLista) (car medioCampistasLista) fitnessMedioCampista) 
                    (obtenerProximaGeneracionMedioCampistas (cdr medioCampistasLista) (cdr hijosMedioCampistasLista))))))

(define (obtenerProximaGeneracionDelanteros delanterosLista hijosDelanterosLista)
    (cond 
        ((null? delanterosLista) 
            '())
        ((> (fitnessDelantero-aux (car delanterosLista)) (fitnessDelantero-aux (car hijosDelanterosLista)))
            (cons (fitnessDelantero (car delanterosLista)) 
                    (obtenerProximaGeneracionDelanteros (cdr delanterosLista) hijosDelanterosLista)))
        (else 
            (cons (cambiarPosicion (car hijosDelanterosLista) (car delanterosLista) fitnessDelantero) 
                    (obtenerProximaGeneracionDelanteros (cdr delanterosLista) (cdr hijosDelanterosLista))))))
(define (cambiarPosicion hijo padre funcionAptitud)
    (funcionAptitud (append (list (car hijo) (cadr hijo) (caddr hijo) (cadddr padre) ) )))

(define (siguienteEquipo EquipoA EquipoB)
    (list (obtenerProximaGeneracionPorteros (car EquipoA) (quicksort- (car(hijosEquipo EquipoA EquipoB)) fitnessPortero-aux)) 
            (obtenerProximaGeneracionDefensas (cadr EquipoA) (quicksort- (cadr( hijosEquipo EquipoA EquipoB)) fitnessDefensa-aux))
            (obtenerProximaGeneracionMedioCampistas (caddr EquipoA) (quicksort- (caddr(hijosEquipo EquipoA EquipoB)) fitnessMedioCampista-aux))
            (obtenerProximaGeneracionDelanteros (cadddr EquipoA) (quicksort- (cadddr (hijosEquipo EquipoA EquipoB)) fitnessDelantero-aux))))
(define (quicksort- hijos funcionAptitud)
    (cond ((null? hijos) 
            '())
    (else
        (quicksort-aux (car hijos) (cdr hijos) funcionAptitud '() '()))))

(define (quicksort-aux pivot lista funcionAptitud menores mayores)
    (cond 
        ((null? lista)
            (append (quicksort- mayores funcionAptitud) (list pivot) (quicksort- menores funcionAptitud)))
        ((< (funcionAptitud pivot) (funcionAptitud (car lista))) 
            (quicksort-aux pivot (cdr lista) funcionAptitud menores (cons (car lista) mayores)))
        (else 
            (quicksort-aux pivot (cdr lista) funcionAptitud (cons (car lista) menores) mayores))))

