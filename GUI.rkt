#lang racket
(require (lib "graphics.ss" "graphics")); Librería de gráficos simples
(open-graphics); Se abre la librería

(define VentanaPrincipal (open-viewport "ConcaChampionsCE" 1000 500)); Se abre la ventana tipo viewport

;Bases gráficas para la interfaz

((draw-viewport VentanaPrincipal)  "green")
((draw-string VentanaPrincipal) (make-posn 10 20 )  "GENERACIÓN:" "black")
((draw-string VentanaPrincipal) (make-posn 380 20 )  "ROJO:" "black")
((draw-string VentanaPrincipal) (make-posn 600 20 )  "AZUL:" "black")

; Marco de la izquierda
((draw-solid-rectangle VentanaPrincipal) (make-posn 0 150 ) 70 10 "black")
((draw-solid-rectangle VentanaPrincipal) (make-posn 0 300 ) 70 10 "black")
((draw-solid-rectangle VentanaPrincipal) (make-posn 70 150 ) 10 160 "black")

;Marco de la derecha
((draw-solid-rectangle VentanaPrincipal) (make-posn 930 150 ) 70 10 "black")
((draw-solid-rectangle VentanaPrincipal) (make-posn 930 300 ) 70 10 "black")
((draw-solid-rectangle VentanaPrincipal) (make-posn 930 150 ) 10 160 "black")

;Línea del centro
((draw-solid-rectangle VentanaPrincipal) (make-posn 500 0 ) 10 150 "black")
((draw-solid-rectangle VentanaPrincipal) (make-posn 500 350 ) 10 150 "black")
((draw-solid-ellipse VentanaPrincipal) (make-posn 400 150 ) 220 220 "black")
((draw-solid-ellipse VentanaPrincipal) (make-posn 415 165 ) 190 190 "green")

;Balón
;((draw-solid-ellipse VentanaPrincipal) (make-posn 490 240 )  30 30 "white")

;Equipo Rojo
((draw-solid-rectangle VentanaPrincipal) (make-posn 20 190 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 140 260 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 140 140 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 200 30 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 200 390 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 250 260 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 250 140 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 300 40 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 300 380 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 400 120 ) 30 60 "red")
((draw-solid-rectangle VentanaPrincipal) (make-posn 400 350 ) 30 60 "red")

;Equipo Azul

((draw-solid-rectangle VentanaPrincipal) (make-posn 960 190 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 860 260 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 860 140 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 790 30 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 790 390 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 700 260 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 700 140 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 650 40 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 650 380 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 550 90 ) 30 60 "blue")
((draw-solid-rectangle VentanaPrincipal) (make-posn 550 370 ) 30 60 "blue")


;Prueba de ciclo del juego

(define (main)
  (let ( (posX (random 900)) (posY (random 400)))
    ((draw-solid-ellipse VentanaPrincipal) (make-posn posX posY )  30 30 "white")
    (sleep 1)
    ((draw-solid-ellipse VentanaPrincipal) (make-posn posX posY )  30 30 "green" )

(main)))


(main)
