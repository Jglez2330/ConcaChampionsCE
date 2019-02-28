#lang racket
;; Funcion que recibe el gen de un individuo  y calcula el fitness de este
;; El gen es una matriz con el primer elemento es la velocidad,  el segundo  es la fuerza, y el tercero es la habilidad
(define (fitnessDelantero genIndividuo)
    (list (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo) (cadddr genIndividuo) (fitnessDelantero-aux genIndividuo)))
(define (fitnessDelantero-aux genIndividuo)
    (+ (car genIndividuo) (cadr genIndividuo) (caddr genIndividuo)))

(define (player)
    (list (+ (random 10) 1) (+ (random 10) 1) (+ (random 10) 1)))

(define (portero)
    (append (player) (list(list 10 10)) '(0)))

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



