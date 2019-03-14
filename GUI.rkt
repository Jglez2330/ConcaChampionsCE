#lang racket
(require (lib "graphics.ss" "graphics")); Librería de gráficos simples
(open-graphics); Se abre la librería
(require racket/format)
(require "GeneticAlgorithm.scm")

(define VentanaPrincipal (open-viewport "ConcaChampionsCE" 1200 650)); Se abre la ventana tipo viewport
(define VentanaOculta (open-pixmap "ConcaChampionsCE" 1200 650));Se abre una ventana tipo pixmap (que permanece oculta), aquí se dibujan los elementos de la interfaz

;Bases gráficas para la interfaz

(define (ColorVentana) ((draw-viewport VentanaOculta)  "green"))
(define (StringGen) ((draw-string VentanaOculta) (make-posn 10 20 )  "GENERACIÓN:" "black"))
(define (StringRojo) ((draw-string VentanaOculta) (make-posn 380 20 )  "ROJO:" "black"))
(define (StringAzul) ((draw-string VentanaOculta) (make-posn 800 20 )  "AZUL:" "black"))
(define (GoalString) ((draw-string VentanaOculta) (make-posn  540 335 )  "GOOOOOOOOOOL" "black"))
(define (StringGeneracionActual contadorGen) ((draw-string VentanaOculta) (make-posn 140 20 )  (~a contadorGen) "black"))

;;Puntuaciones
(define (Puntuacion puntosA puntosR) (PuntuacionRojo puntosR) (PuntuacionAzul puntosA))
(define (PuntuacionRojo puntos) ((draw-string VentanaOculta) (make-posn 440 20 )  (~a puntos) "black"))
(define (PuntuacionAzul puntos) ((draw-string VentanaOculta) (make-posn 860 20 )  (~a puntos) "black"))

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

;Equipo 1 (Color Rojo)
(define (JugadorR posX posY) ((draw-solid-rectangle VentanaOculta) (make-posn posX posY ) 24 35 "red"))

;Equipo 2 (Color Azul)

(define (JugadorA posX posY) ((draw-solid-rectangle VentanaOculta) (make-posn posX posY ) 24 35 "blue"))


;Variables globales importantes

(define CantGeneraciones 0)
(define (EstablecerCantGeneraciones cantGeneraciones) (set! CantGeneraciones cantGeneraciones) CantGeneraciones)
(define PrimeraGeneracion '())
(define (EstablecerPrimeraGeneracion primeraGeneracion) (set! PrimeraGeneracion primeraGeneracion) PrimeraGeneracion)
(define CantDelanteros1 0)
(define CantDelanteros2 0)
(define (EstablecerCantDelanteros1 cantDelanteros1) (set! CantDelanteros1 cantDelanteros1) CantDelanteros1)
(define (EstablecerCantDelanteros2 cantDelanteros2) (set! CantDelanteros2 cantDelanteros2) CantDelanteros2)
(define EsGol #f)
(define (EstablecerEsGol esGol) (set! EsGol esGol) EsGol)
(define CantMedios1 0)
(define CantMedios2 0)
(define (EstablecerCantMedios1 cantMedios1) (set! CantMedios1 cantMedios1) CantMedios1)
(define (EstablecerCantMedios2 cantMedios2) (set! CantMedios2 cantMedios2) CantMedios2)


;Se definen las funciones para estar dibujando los elementos en cada ciclo del juego

;Se dibujan los elementos básicos del terreno de juego
(define (ElementosBasicos) (list (ColorVentana)(StringGen)(StringRojo)(StringAzul)))
(define (MarcoIzquierda) (list (PosteSuperiorMI)(PosteInferiorMI)(LineaGolMI)))
(define (MarcoDerecha) (list (PosteSuperiorMD)(PosteInferiorMD)(LineaGolMD)))
(define (CentroCancha) (list (LineaSuperiorLC)(LineaInferiorLC)(CirculoNegroLC)(CirculoVerdeLC)))
(define (DibujarCancha) (begin (ElementosBasicos)(MarcoIzquierda)(MarcoDerecha)(CentroCancha)))

;Se dibuja el número actual de generaciones
(define ContadorGen 0)
(define (IncrementoContadorGen) (set! ContadorGen (+ 1 ContadorGen)) ContadorGen)
(define (DibujarGeneraciones numeroGeneraciones) (StringGeneracionActual numeroGeneraciones))

;Se dibuja la puntuación actual del juego
(define PuntosA 0)
(define PuntosR 0)
(define (IncrementoPuntosA) (set! PuntosA (+ 1 PuntosA)) PuntosA)
(define (IncrementoPuntosR) (set! PuntosR (+ 1 PuntosR)) PuntosR)
(define (DibujarPuntuacion puntosA puntosR) (Puntuacion puntosA puntosR))

;Se dibuja el balón del juego
(define (DibujarBalon posX posY)  (cond
((even? (quotient ContadorGen 2) ) ((draw-solid-ellipse VentanaOculta) (make-posn posX posY )  30 30 "white")
((draw-solid-rectangle VentanaOculta) (make-posn (+ posX 12) (+ posY 1) ) 5 28 "black"))
(else ((draw-solid-ellipse VentanaOculta) (make-posn posX posY )  30 30 "white")
((draw-solid-rectangle VentanaOculta) (make-posn (+ posX 1) (+ posY 12) ) 28 5 "black"))))
(define PosXBalon 595)
(define PosYBalon 315)
(define VelXBalon -10)
(define VelYBalon -10)
(define (NuevaPosXBalon nuevaPosX) (set! PosXBalon nuevaPosX) PosXBalon)
(define (NuevaPosYBalon nuevaPosY) (set! PosYBalon nuevaPosY) PosYBalon)
(define (NuevaVelXBalon nuevaVelX) (set! VelXBalon nuevaVelX) VelXBalon)
(define (NuevaVelYBalon nuevaVelY) (set! VelYBalon nuevaVelY) VelYBalon)

;Se dibujan los jugadores de cada equipo

(define (DibujarJugadores listaJugadores contador contador2)
  (cond ((null? listaJugadores) #t)
        ((equal? contador 12) (begin (JugadorA (car (ObtenerPosJugador (car listaJugadores))) (cadr (ObtenerPosJugador (car listaJugadores))))
        ((draw-string VentanaOculta) (make-posn (+ (car (ObtenerPosJugador (car listaJugadores))) 4) (+ (cadr (ObtenerPosJugador (car listaJugadores))) 17.5) )  (~a contador2) "white")
        (DibujarJugadores (cdr listaJugadores) contador (+ 1 contador2)) ))
        (else (begin (JugadorR (car (ObtenerPosJugador (car listaJugadores))) (cadr (ObtenerPosJugador (car listaJugadores))))
        ((draw-string VentanaOculta) (make-posn (+ (car (ObtenerPosJugador (car listaJugadores)))4) (+ (cadr (ObtenerPosJugador (car listaJugadores)))17.5) )  (~a contador2) "white")             
        (DibujarJugadores (cdr listaJugadores) (+ 1 contador) (+ 1 contador2) ) ))
        ))

;Verificaciones de la lógica de juego

;Función que se encarga de verificar si el balón rebotó en alguno de los bordes
(define (VerificarReboteBalon)
  (cond ((< PosYBalon 0) (begin (NuevaPosXBalon (+ PosXBalon VelXBalon)) (NuevaPosYBalon 5) (NuevaVelYBalon (* VelYBalon -1))));Rebotar con el borde superior
        ((< PosXBalon 0) (begin (NuevaPosXBalon 5) (NuevaPosYBalon (+ PosYBalon VelYBalon)) (NuevaVelXBalon (* VelXBalon -1))));Rebotar con el borde izquierdo
        ((> PosYBalon 620) (begin (NuevaPosXBalon (+ PosXBalon VelXBalon)) (NuevaPosYBalon 615) (NuevaVelYBalon (* VelYBalon -1))));Rebotar con el borde inferior
        ((> PosXBalon 1170) (begin (NuevaPosXBalon 1165) (NuevaPosYBalon (+ PosYBalon VelYBalon)) (NuevaVelXBalon (* VelXBalon -1))))));Rebotar con el borde derecho

;Función que se encarga de verificar si el balón ingreso en alguno de los marcos
(define (VerificarGol nuevosEquipos)
  (cond ((and (<  PosXBalon 33) (> PosYBalon 260) (< PosYBalon 390));Gol del lado izquierdo
               (EstablecerEsGol #t)
               (DibujarCancha)
               (DibujarJugadores (ResetearPosiciones PrimeraGeneracion nuevosEquipos) 1 1)
               (GoalString)
               (copy-viewport VentanaOculta VentanaPrincipal)
               (sleep 2)
               (IncrementoPuntosA)
               (NuevaPosXBalon 700)
               (NuevaPosYBalon 310))
        ((and (> PosXBalon 1170) (> PosYBalon 280) (< PosYBalon 370));Gol del lado derecho
               (EstablecerEsGol #t)
               (DibujarCancha)
               (DibujarJugadores (ResetearPosiciones PrimeraGeneracion nuevosEquipos) 1 1)
               (GoalString)
               (copy-viewport VentanaOculta VentanaPrincipal)
               (sleep 2)
               (IncrementoPuntosR)
               (NuevaPosXBalon 700)
               (NuevaPosYBalon 310))))

;Función que se encarga de obtener la Posición de cada jugador
(define (ObtenerPosJugador jugador) (cadddr jugador))

;Función que se encarga de obtener la velocidad de un jugador
(define (ObtenerVelJugador jugador) (car jugador))

;Función que se encarga de obtener la fuerza de un jugador 
(define (ObtenerFuerzaJugador jugador) (cadr jugador))

;Función que se encarga de cambiar la posición de un jugador, el índice debe comenzar en 1
(define (CambiarPosJugador jugador nuevaPos indice)
  (cond ((null? jugador) '() )
        ((equal? indice 4) (cons nuevaPos (CambiarPosJugador (cdr jugador) nuevaPos (+ 1 indice))))
        (else (cons (car jugador) (CambiarPosJugador (cdr jugador) nuevaPos (+ 1 indice))))))

;Función que recibe la lista con los dos equipos y me devuelve una sola lista con todos los jugadores

(define (PegarEquipos equipo1 equipo2)
  (append (car equipo1) (cadr equipo1) (caddr equipo1) (cadddr equipo1)(car equipo2) (cadr equipo2) (caddr equipo2) (cadddr equipo2) ))

;Función que verifica las colisiones de dos jugadores

(define (colisionJugadores? jugador1 jugador2)
  (let* ( (posX1 (car (ObtenerPosJugador jugador1))) (posX2 (car (ObtenerPosJugador jugador2)))
          (posY1 (cadr (ObtenerPosJugador jugador1))) (posY2 (cadr(ObtenerPosJugador jugador2)))
          (L1 (- posX1 12)) (R1 (+ posX1 12)) (L2 (- posX2 12)) (R2 (+ posX2 12))
          (A1 (- posY1 17.5)) (B1 (+ posY1 17.5)) (A2 (- posY2 17.5)) (B2 (+ posY2 17.5)) ) 
    (cond ((and (< L2 R1) (< L1 R2) (< A2 B1) (< A1 B2)) #t)
          (else #f))))

;Función que verifica la colisión de un jugador y el balón
(define (colisionJugadorBalon? jugador)
  (let* ( (posXJugador (car (ObtenerPosJugador jugador))) (posYJugador (cadr (ObtenerPosJugador jugador)))
           (DeltaX (- PosXBalon (max posXJugador (min PosXBalon (+ posXJugador 24)) )))
           (DeltaY (- PosYBalon (max posYJugador (min PosYBalon (+ posYJugador 35)))) ))
           (cond ( (< (+ (* DeltaX  DeltaX) (* DeltaY DeltaY) ) (* 15 15) ) #t)
           (else #f))))

;Función que verifica si un jugador esta colisionando con otro en el campo 
(define (VerificarColisiones Equipos)
  (let ((listaJugadores (PegarEquipos (car Equipos) (cadr Equipos)))) (VerificarColisiones2 listaJugadores)))

(define (VerificarColisiones2 listaJugadores)
  (cond ((null? listaJugadores) '())
        (else (cons (VerificarColisionJugadorLista (car listaJugadores) (cdr listaJugadores)) (VerificarColisiones2 (cdr listaJugadores))))))

(define (VerificarColisionJugadorLista jugador listaJugadores)
  (cond ((null? listaJugadores) jugador)
        ((equal? #t (colisionJugadores? jugador (car listaJugadores))) (VerificarColisionJugadorLista (ArreglarPosJugador jugador) (cdr listaJugadores)))
        (else (VerificarColisionJugadorLista jugador (cdr listaJugadores) ))))

(define (ArreglarPosJugador jugador)
  (let ( (posX (car (ObtenerPosJugador jugador))) (posY (cadr(ObtenerPosJugador jugador))) )
    (cond ((> posY 575) (CambiarPosJugador jugador (list posX (- posY 50) ) 1))
          (else (CambiarPosJugador jugador (list posX (+ posY 40) ) 1))
          )))

;Función que verifica si un jugador esta en condición de disparar el balón

(define (VerificarDisparo Equipos)
  (let ((listaJugadores (PegarEquipos (car Equipos) (cadr Equipos)))) (VerificarDisparo-aux (car listaJugadores) (cdr listaJugadores) 1)))

(define (VerificarDisparo-aux jugador listaJugadores contador)
  (cond ((null? listaJugadores) #f)
  ((and (colisionJugadorBalon? jugador) (<= contador 11) (> contador (- 11 CantDelanteros1)) )
  (begin (NuevaVelXBalon (ApuntarX PosXBalon PosYBalon 1200 (random 280 370) (ObtenerFuerzaJugador jugador))) (NuevaVelYBalon (ApuntarY PosXBalon PosYBalon 1200 (random 280 370) (ObtenerFuerzaJugador jugador))) ))
  ((and (colisionJugadorBalon? jugador) (<= contador 11) (<= contador (- 11 CantDelanteros1)) )
  (begin (NuevaVelXBalon (ApuntarX PosXBalon PosYBalon 1200 (random 0 650) (ObtenerFuerzaJugador jugador))) (NuevaVelYBalon (ApuntarY PosXBalon PosYBalon 1200 (random 0 650) (ObtenerFuerzaJugador jugador))) ))
  ((and (colisionJugadorBalon? jugador) (>= contador 12) (> contador (- 22 CantDelanteros2)) )
  (begin (NuevaVelXBalon (ApuntarX PosXBalon PosYBalon 0 (random 270 430) (ObtenerFuerzaJugador jugador))) (NuevaVelYBalon (ApuntarY PosXBalon PosYBalon 0 (random 270 430) (ObtenerFuerzaJugador jugador))) ))
  ((and (colisionJugadorBalon? jugador) (>= contador 12) (<= contador (- 22 CantDelanteros2)) )
  (begin (NuevaVelXBalon (ApuntarX PosXBalon PosYBalon 0 (random 0 650) (ObtenerFuerzaJugador jugador))) (NuevaVelYBalon (ApuntarY PosXBalon PosYBalon 0 (random 0 650) (ObtenerFuerzaJugador jugador))) ))
  (else (VerificarDisparo-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador)))))


;Funciones para apuntar, devuelven la velocidad X y Y para llegar al punto obj
(define(ApuntarX posX posY objX objY fuerza)
  (cond (zero? (- objY posY)
               (cond ((> objX posX) fuerza)
                     (else (* -1 fuerza))))
        (else
         (cond ((> objX posX) (*(/ (abs (- objX posX)) (abs (- objY posY))) fuerza))
                     (else (* -1 (*(/ (abs (- objX posX)) (abs (- objY posY))) fuerza)))))))

(define(ApuntarY posX posY objX objY fuerza)
  (cond ((< objY posY) (* -1 (* (/ (abs(- objY posY)) (abs(- objX posX))) fuerza)));;
        (else (* (/ (abs(- objY posY)) (abs(- objX posX))) fuerza))))

;Función para resetear las posiciones de los jugadores de las nuevas generaciones en cada gol

(define (ResetearPosiciones primeraGeneracion nuevaGeneracion)
  (let ((listaPrimeraGeneracion (PegarEquipos (car primeraGeneracion) (cadr primeraGeneracion)))
        (listaNuevaGeneracion (PegarEquipos (car nuevaGeneracion) (cadr nuevaGeneracion))))
        (ResetearPosiciones-aux listaPrimeraGeneracion listaNuevaGeneracion)))

(define (ResetearPosiciones-aux listaPrimeraGeneracion listaNuevaGeneracion)
  (cond ((null? listaPrimeraGeneracion) '())
        (else (cons (CambiarPosJugador (car listaNuevaGeneracion) (ObtenerPosJugador (car listaPrimeraGeneracion)) 1)
                    (ResetearPosiciones-aux (cdr listaPrimeraGeneracion) (cdr listaNuevaGeneracion))))))

;;Funcion que mueve los jugadores
(define (MoverJugadores Equipos)
  (let ((listaJugadores (PegarEquipos (car Equipos) (cadr Equipos)))) (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) 1 '())))

(define (MoverJugadores-aux jugador listaJugadores contador nuevaLista)
  (cond ((null? listaJugadores) (Invertir nuevaLista '()))
        ;;Equipo Rojo
        ((= contador 1) ;;Portero Rojo
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosPorteroRojo jugador) nuevaLista)))
        ((and (<= contador 11) (<= contador (- 11 CantDelanteros1 CantMedios1))) ;;Defensas rojos
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosDefRojo jugador) nuevaLista)))
        ((and (<= contador 11) (<= contador (- 11 CantDelanteros1))) ;;Medios rojos
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosMedRojo jugador) nuevaLista)))
        ((and (<= contador 11)) ;;Delanteros rojos
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosDelRojo jugador) nuevaLista)))
        ;;Equipo azul
        ((= contador 12) ;;Portero azul
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosPorteroAzul jugador) nuevaLista)))
        ((and (<= contador 11) (<= contador (- 22 CantDelanteros1 CantMedios1))) ;;Defensas azul
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosDefAzul jugador) nuevaLista)))
        ((and (<= contador 11) (<= contador (- 22 CantDelanteros2))) ;;Medios azul
               (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosMedAzul jugador) nuevaLista)))
        (else ;;Delanteros azul
              (MoverJugadores-aux (car listaJugadores) (cdr listaJugadores) (+ 1 contador) (cons (PosDelAzul jugador) nuevaLista)))
        ))

(define (Invertir lista nuevaLista)
  (cond ((null? lista) nuevaLista)
        (else (Invertir (cdr lista) (cons (car lista) nuevaLista)))))


;;Funciones para averiguar la nueva posicion de todos los jugadores        
(define (PosPorteroRojo jugador)
  (cond((< (cadr (ObtenerPosJugador jugador)) 260) (CambiarPosJugador jugador (cons (car (ObtenerPosJugador jugador)) 270) 1));;Limite superior
                    ((> (cadr (ObtenerPosJugador jugador)) 390) (CambiarPosJugador jugador (cons (car (ObtenerPosJugador jugador)) 380) 1));;Limite inferior
                    ((> (car (ObtenerPosJugador jugador)) 70) (CambiarPosJugador jugador (cons 60 (cadr (ObtenerPosJugador jugador))) 1));;Limite hacia el centro
                    (else  (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1))))

(define (PosDefRojo jugador)
         (cond ((> (car (ObtenerPosJugador jugador)) 400) (CambiarPosJugador jugador (cons 390 (cadr (ObtenerPosJugador jugador))) 1))
         (else (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)))) 
(define (PosMedRojo jugador)
  (cond ((> (car (ObtenerPosJugador jugador)) 800) (CambiarPosJugador jugador (cons 790 (cadr (ObtenerPosJugador jugador))) 1))
        (else (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)))) 

(define (PosDelRojo jugador)
        (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)) 

(define (PosPorteroAzul jugador)
  (cond((< (cadr (ObtenerPosJugador jugador)) 260) (CambiarPosJugador jugador (cons (car (ObtenerPosJugador jugador)) 270) 1));;Limite superior
       ((> (cadr (ObtenerPosJugador jugador)) 390) (CambiarPosJugador jugador (cons (car (ObtenerPosJugador jugador)) 380) 1));;Limite inferior
       ((< (car (ObtenerPosJugador jugador)) 1130) (CambiarPosJugador jugador (cons 1140 (cadr (ObtenerPosJugador jugador))) 1));;Limite hacia el centro
       (else  (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1))))

(define (PosDefAzul jugador)
       (cond ((< (car (ObtenerPosJugador jugador)) 800) (CambiarPosJugador jugador (cons 810 (cadr (ObtenerPosJugador jugador))) 1))
       (else (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)))) 

(define (PosMedAzul jugador)
  (cond ((< (car (ObtenerPosJugador jugador)) 400) (CambiarPosJugador jugador (cons 410 (cadr (ObtenerPosJugador jugador))) 1))
        (else (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)))) 

(define (PosDelAzul jugador)
   (CambiarPosJugador jugador (list (+ (car (ObtenerPosJugador jugador))(ApuntarX (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador))) (+ (cadr(ObtenerPosJugador jugador)) (ApuntarY (car(ObtenerPosJugador jugador)) (cadr (ObtenerPosJugador jugador)) PosXBalon PosYBalon (ObtenerVelJugador jugador)))) 1)) 



;Función que verifica la condición de finalización

(define (VerificarFinal)
  (cond ((equal? CantGeneraciones ContadorGen) #t)
        ((and (> PuntosA PuntosR) (equal? 3 (- PuntosA PuntosR)) ) #t)
        ((and (< PuntosA PuntosR) (equal? 3 (- PuntosR PuntosA)) ) #t)
        (else #f)))


;Función principal del juego
(define (CCCE2019 formacion1 formacion2 cantGeneraciones)
  (EstablecerCantGeneraciones cantGeneraciones)
  (EstablecerPrimeraGeneracion (list (primeraGeneracionEquipoIzquierda formacion1) (primeraGeneracionEquipoDerecha formacion2)))
  (DibujarPrimeraGeneracion PrimeraGeneracion))



;Función que me dibuja la primera generación
(define (DibujarPrimeraGeneracion equipos)
   ;Se dibujan los elementos de la interfaz
  (DibujarCancha)
  (DibujarGeneraciones (IncrementoContadorGen))
  (DibujarPuntuacion PuntosA PuntosR)
  (DibujarBalon PosXBalon PosYBalon)
  (DibujarJugadores (VerificarColisiones equipos) 1 1)
  (copy-viewport VentanaOculta VentanaPrincipal);Se dibuja en la ventana oculta y se copia en la principal, con el objetivo de evitar el parpadeo de la pantalla en cada frame del ciclo
  (sleep 1)
  (Main equipos))

;Ciclo del Juego
(define (Main equipos)
  
  ;Se obtiene la nueva generación de equipos
  (define equipoNuevo1 (siguienteEquipo (car equipos) (cadr equipos)))
  (define equipoNuevo2 (siguienteEquipo (car equipos) (cadr equipos)))
  (EstablecerCantDelanteros1 (length (cadddr equipoNuevo1)))
  (EstablecerCantDelanteros2 (length (cadddr equipoNuevo2)))
  (EstablecerCantMedios1 (length (caddr equipoNuevo1)))
  (EstablecerCantMedios2 (length (caddr equipoNuevo2)))
  (define nuevosEquipos (list equipoNuevo1 equipoNuevo2))
  
  ;Se verifican las condiciones de la logica
  (if (equal? #t (VerificarFinal)) (Final) #f) 
  (VerificarGol nuevosEquipos)
  (VerificarReboteBalon)
  (VerificarDisparo nuevosEquipos)
   
  ;Se dibujan los elementos de la interfaz 
  (DibujarCancha)
  (DibujarGeneraciones (IncrementoContadorGen))
  (DibujarPuntuacion PuntosA PuntosR)
  (DibujarBalon PosXBalon PosYBalon)
  (cond ((equal? EsGol #t) (EstablecerEsGol #f) )
        (else (DibujarJugadores (VerificarColisiones nuevosEquipos) 1 1)))
   
       
        
  
  ;Se hace la llamada recursiva
  (copy-viewport VentanaOculta VentanaPrincipal);Se dibuja en la ventana oculta y se copia en la principal, con el objetivo de evitar el parpadeo de la pantalla en cada frame del ciclo
  (sleep 0.01)
  (NuevaPosXBalon (+ PosXBalon VelXBalon))
  (NuevaPosYBalon (+ PosYBalon VelYBalon))
  (Main nuevosEquipos)) 

;Función para el caso donde termina el juego 

(define (Final)
   (sleep 2) (close-viewport VentanaPrincipal))


;Se invoca al programa
(CCCE2019 '(4 4 2) '(4 4 2) 100000)