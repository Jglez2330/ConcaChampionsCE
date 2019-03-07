#lang racket
(require (lib "graphics.ss" "graphics")); Librería de gráficos simples
(open-graphics); Se abre la librería

(define VentanaPrincipal (open-viewport "ConcaChampionsCE" 1200 650)); Se abre la ventana tipo viewport
(define VentanaOculta (open-pixmap "ConcaChampionsCE" 1200 650));Se abre una ventana tipo pixmap (que permanece oculta), aquí se dibujan los elementos de la interfaz

;Bases gráficas para la interfaz

(define (ColorVentana) ((draw-viewport VentanaOculta)  "green"))
(define (StringGen) ((draw-string VentanaOculta) (make-posn 10 20 )  "GENERACIÓN:" "black"))
(define (StringRojo) ((draw-string VentanaOculta) (make-posn 380 20 )  "ROJO:" "black"))
(define (StringAzul) ((draw-string VentanaOculta) (make-posn 800 20 )  "AZUL:" "black"))

;Marco Izquierda (MI)
(define (PosteSuperiorMI) ((draw-solid-rectangle VentanaOculta) (make-posn 0 250 ) 70 10 "black"))
(define (PosteInferiorMI) ((draw-solid-rectangle VentanaOculta) (make-posn 0 400 ) 70 10 "black"))
(define (LineaGolMI) ((draw-solid-rectangle VentanaOculta) (make-posn 70 250 ) 10 160 "black"))

;Marco de la derecha (MD)
(define (PosteSuperiorMD) ((draw-solid-rectangle VentanaOculta) (make-posn 1130 250 ) 70 10 "black"))
(define (PosteInferiorMD) ((draw-solid-rectangle VentanaOculta) (make-posn 1130 400 ) 70 10 "black"))
(define (LineaGolMD) ((draw-solid-rectangle VentanaOculta) (make-posn 1130 250 ) 10 160 "black"))

;Línea del centro (LC)
(define (LineaSuperiorLC) ((draw-solid-rectangle VentanaOculta) (make-posn 600 0 ) 10 225 "black"))
(define (LineaInferiorLC) ((draw-solid-rectangle VentanaOculta) (make-posn 600 430 ) 10 340 "black"))
(define (CirculoNegroLC) ((draw-solid-ellipse VentanaOculta) (make-posn 500 225 ) 220 220 "black"))
(define (CirculoVerdeLC) ((draw-solid-ellipse VentanaOculta) (make-posn 515 240 ) 190 190 "green"))

;Balón
;(define (Balon) ((draw-solid-ellipse VentanaPrincipal) (make-posn 590 310 )  30 30 "white"))

;Equipo 1 (Color Rojo)
;En el nombre de los jugadores el primer índice indica el equipo y el segundo el número de jugador 
(define (Jugador11) ((draw-solid-rectangle VentanaOculta) (make-posn 20 300 ) 30 60 "red"))
(define (Jugador12) ((draw-solid-rectangle VentanaOculta) (make-posn 140 140 ) 30 60 "red"))
(define (Jugador13) ((draw-solid-rectangle VentanaOculta) (make-posn 140 420 ) 30 60 "red"))
(define (Jugador14) ((draw-solid-rectangle VentanaOculta) (make-posn 200 30 ) 30 60 "red"))
(define (Jugador15) ((draw-solid-rectangle VentanaOculta) (make-posn 200 530 ) 30 60 "red"))
(define (Jugador16) ((draw-solid-rectangle VentanaOculta) (make-posn 290 180 ) 30 60 "red"))
(define (Jugador17) ((draw-solid-rectangle VentanaOculta) (make-posn 290 380 ) 30 60 "red"))
(define (Jugador18) ((draw-solid-rectangle VentanaOculta) (make-posn 400 40 ) 30 60 "red"))
(define (Jugador19) ((draw-solid-rectangle VentanaOculta) (make-posn 400 520 ) 30 60 "red"))
(define (Jugador110) ((draw-solid-rectangle VentanaOculta) (make-posn 480 220 ) 30 60 "red"))
(define (Jugador111) ((draw-solid-rectangle VentanaOculta) (make-posn 480 380 ) 30 60 "red"))

;Equipo 2 (Color Azul)

(define (Jugador21) ((draw-solid-rectangle VentanaOculta) (make-posn 1160 300 ) 30 60 "blue"))
(define (Jugador22) ((draw-solid-rectangle VentanaOculta) (make-posn 1040 140 ) 30 60 "blue"))
(define (Jugador23) ((draw-solid-rectangle VentanaOculta) (make-posn 1040 420 ) 30 60 "blue"))
(define (Jugador24) ((draw-solid-rectangle VentanaOculta) (make-posn 990 30 ) 30 60 "blue"))
(define (Jugador25) ((draw-solid-rectangle VentanaOculta) (make-posn 990 530 ) 30 60 "blue"))
(define (Jugador26) ((draw-solid-rectangle VentanaOculta) (make-posn 890 180 ) 30 60 "blue"))
(define (Jugador27) ((draw-solid-rectangle VentanaOculta) (make-posn 890 380 ) 30 60 "blue"))
(define (Jugador28) ((draw-solid-rectangle VentanaOculta) (make-posn 780 40 ) 30 60 "blue"))
(define (Jugador29) ((draw-solid-rectangle VentanaOculta) (make-posn 780 520 ) 30 60 "blue"))
(define (Jugador210) ((draw-solid-rectangle VentanaOculta) (make-posn 710 220 ) 30 60 "blue"))
(define (Jugador211) ((draw-solid-rectangle VentanaOculta) (make-posn 710 380 ) 30 60 "blue"))

;Se definen las funciones para estar dibujando los elementos en cada ciclo del juego
(define (ElementosBasicos) (list (ColorVentana)(StringGen)(StringRojo)(StringAzul)))
(define (MarcoIzquierda) (list (PosteSuperiorMI)(PosteInferiorMI)(LineaGolMI)))
(define (MarcoDerecha) (list (PosteSuperiorMD)(PosteInferiorMD)(LineaGolMD)))
(define (CentroCancha) (list (LineaSuperiorLC)(LineaInferiorLC)(CirculoNegroLC)(CirculoVerdeLC)))
(define (Equipo1)(list (Jugador11)(Jugador12)(Jugador13)(Jugador14)(Jugador15)(Jugador16)(Jugador17)(Jugador18)(Jugador19)(Jugador110)(Jugador111)))
(define (Equipo2)(list (Jugador21)(Jugador22)(Jugador23)(Jugador24)(Jugador25)(Jugador26)(Jugador27)(Jugador28)(Jugador29)(Jugador210)(Jugador211)))
(define (DibujarCancha) (begin (ElementosBasicos)(MarcoIzquierda)(MarcoDerecha)(CentroCancha)(Equipo1)(Equipo2)))

;Ciclo del Juego
(define (Main)
  (let ( (posX (random 1100)) (posY (random 500)))
    (DibujarCancha)
    ((draw-solid-ellipse VentanaOculta) (make-posn posX posY )  30 30 "white");Se prueba un movimiento básico del balón
    (copy-viewport VentanaOculta VentanaPrincipal);Se dibuja en la ventana oculta y se copia en la principal, con el objetivo de evitar el parpadeo de la pantalla en cada frame del ciclo
    (sleep 1)
(Main)))

(Main)
