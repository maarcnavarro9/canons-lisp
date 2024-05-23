"MARC NAVARRO AMENGUAL & JOAN MATEU SOCIAS PALMER"

(defun inicia ()

    "PROPIETATS"

    "escenari"
    (putprop 'escenari 340 'alsada)
    (putprop 'escenari 640 'amplada)

    "mur"
    (putprop 'mur (obtenir-valor-aleatori 20 40) 'amplada)
    (putprop 'mur (obtenir-valor-aleatori 100 150) 'alsada)

    "camp esquerra"
    (putprop 'campEsquerra 0 'iniciCamp)    
    (putprop 'campEsquerra (+ (round (/ (- (get 'escenari 'amplada) (get 'mur 'amplada)) 2)) (obtenir-valor-aleatori 0 40)) 'finalCamp) 
    (putprop 'campEsquerra (obtenir-valor-aleatori 15 45) 'alsada)

    "continuació mur"
    (putprop 'mur (get 'campEsquerra 'finalCamp) 'posicioInicial) 

    "camp dret"
    (putprop 'campDret (+ (get 'campEsquerra 'finalCamp) (get 'mur 'amplada)) 'iniciCamp)
    (putprop 'campDret (+ (get 'campDret 'iniciCamp) (round (/ (- (get 'escenari 'amplada) (get 'mur 'amplada)) 2))) 'finalCamp)
    (putprop 'campDret (obtenir-valor-aleatori 15 45) 'alsada)

    "CANONS"
    (putprop 'cano 20 'amplada)
    (putprop 'cano 10 'alsada) 

    "canó esquerra"
    (putprop 'canoEsquerra (obtenir-valor-aleatori (round (/ (get 'campEsquerra 'finalCamp) 3)) (round (* 2 (/ (get 'campEsquerra 'finalCamp) 3)))) 'posicioInicial)    
    (putprop 'canoEsquerra 45 'angle)

    "canó dret"
    (putprop 'canoDret (obtenir-valor-aleatori (+ (get 'campDret 'iniciCamp) (round (/ (- (get 'escenari 'amplada) (get 'campDret 'iniciCamp)) 3))) (round (+ (* 2 (/ (- (get 'escenari 'amplada) (get 'campDret 'iniciCamp)) 3)) (get 'campDret 'iniciCamp)))) 'posicioInicial)
    (putprop 'canoDret 135 'angle)

    "DISPAR"
    (putprop 'dispar 20 'potenciaDisparDret)
    (putprop 'dispar 20 'potenciaDisparEsquerra)
    (putprop 'dispar 0 'temps)
    (putprop 'dispar 500 'rang)
    (putprop 'dispar 0 'tempsA)
    
    "bala"
    (putprop 'bala 0 'vx)
    (putprop 'bala 0 'vy)
    (putprop 'bala 0 'vyInicial)  

    "vent"
    (putprop 'vent (obtenir-valor-aleatori 0 1) 'direccioVent)  
    (putprop 'vent (obtenir-valor-aleatori 0 5) 'potenciaVent)

    "VARIABLES NECESSÀRIES"

    "gravetat"
    (putprop 'variable -9.8 'gravetat)
    (putprop 'xoc 0 'valorXoc)

    (menu)
)

; funció que deplega un menu
(defun menu ()
    (cls)
    (princ "SELECCIONA MODE DE JOC [1|2]\n\n")
    (princ "1. SENSE DISSENY GRAFIC\n")
    (princ "2. AMB DISSENY GRAFIC\n")
    (setq tecla (get-key))
    (cond ((equal tecla 49) ; 1
           (color 0 0 0)
           (pinta) (repeteix 0))
          ((equal tecla 50) ; 2
           (pinta-color) (repeteix 1) t)
    )
)

"GESTIO DEL VENT"

; funcio que retorna el valor del vent depenent de a on vagi dirigida la bandera"
(defun vent ()
  (cond
    ((equal (get 'vent 'direccioVent) 0) (get 'vent 'potenciaVent))
    (t (- 0 (get 'vent 'potenciaVent)))
  )
)

"FUNCIOS ADICIONALS TRAJECTÒRIA"

; funcions que inicialitzen la velocitat inicial de vx i vy, s'ha fet aixi ja que queda mes net utilitzar aquestes funcions en comptes de posar els
; putprop a les zones de codi on els necesitam (que en el nostre cas es a la funcio repeteix)"
(defun viX (angle pot) (putprop 'bala (* pot (cos (radians angle))) 'vx))
(defun viY (angle pot) (putprop 'bala (* pot (sin (radians angle))) 'vy) (putprop 'bala (* pot (sin (radians angle))) 'vyInicial))

; funció que calcula la velocitat de vy la qual anira variant amb el temps
(defun vy (temps) (putprop 'bala (+ (get 'bala 'vy) (* (get 'variable 'gravetat) temps)) 'vy)
    (cond (t (get 'bala 'vy)))
)

; funció que calvula la velocitat vx que es vora afectada pel temps i pel vent 
(defun vx (temps vent) (putprop 'bala (+ (get 'bala 'vx) (* vent temps)) 'vx)
    (cond (t (get 'bala 'vx)))
)

; funció que ens calcula el temps que tarda la trajectoria en obtenir velocitat 0, el temps calculat es multiplica per dos
; ja que aquest sera el temps resultat que tardi la bala a arribar a la posicio x final
(defun calculTemps ()
    (putprop 'dispar (* 2 (/ (get 'bala 'vyInicial) (abs (get 'variable 'gravetat)))) 'temps)
)

; funció que serveix per dividir el temps en petits troços y aixi que la trajectoria sigui mes nenta i visible
(defun calculRang ()
    (putprop 'dispar (/ (get 'dispar 'temps) (get 'dispar 'rang)) 'tempsA)
)

; funció que calcula la trajectoria de la bala, aquesta funcio va modificant els valors de x y i temps que seran els responsables 
; de la trajectoria de la bala. La condicio d'aturada de la funcio es que la bala no hagui xocat en cap superficie del mapa
(defun trajectoria (x y temps vent)                    
    (putprop 'xoc (xocMapa x y) 'valorXoc)    
    (sleep 0.025) 
    (cond 
        ((not (equal (xocMapa x y) 0)) t)
        (t (drawr x y) (trajectoria
                (+ x (* temps (vx temps vent)))
                (+ y (* temps (vy temps)) (* 0.5 (get 'variable 'gravetat) (expt temps 2)))
                (+ temps (/ (get 'dispar 'tempsA) 50))  
                vent                              
            )
        )
    )
)

"FUNCIONS DE DISSENY GRÀFIC"

(defun escenari ()
    (rectangle 0 0 (get 'escenari 'amplada) (get 'escenari 'alsada))
)

(defun mur ()
    (rectangle (get 'mur 'posicioInicial) 0 (get 'mur 'amplada) (get 'mur 'alsada))
)

(defun bandera ()
    (move (round (+ (get 'mur 'posicioInicial) (/ (float (get 'mur 'amplada)) 2))) (get 'mur 'alsada))
    (drawrel 0 20)
    "verificar si hi ha vent i en cas afirmatiu cap a quina direcció va"
    (cond ((and (/= (get 'vent 'potenciaVent) 0) (equal (get 'vent 'direccioVent) 0)) (trocosBandera (round (+ (get 'mur 'posicioInicial) (/ (float (get 'mur 'amplada)) 2))) (+ (get 'mur 'alsada) 20) 16 8 (get 'vent 'potenciaVent))) ; bandera cap a la dreta
          ((and (/= (get 'vent 'potenciaVent) 0) (equal (get 'vent 'direccioVent) 1)) (trocosBandera (round (+ (get 'mur 'posicioInicial) (/ (float (get 'mur 'amplada)) 2))) (+ (get 'mur 'alsada) 20) -16 8 (get 'vent 'potenciaVent)))) ; bandera cap a l'esquerra
)

(defun campEsquerra () (rectangle 0 0 (get 'campEsquerra 'finalCamp) (get 'campEsquerra 'alsada))

)

(defun campDret () (rectangle (get 'campDret 'iniciCamp) 0 (get 'campDret 'finalCamp) (get 'campDret 'alsada))

)

(defun canoDret () 
    (rectangle (get 'canoDret 'posicioInicial) (get 'campDret 'alsada) (get 'cano 'amplada) (get 'cano 'alsada))        
)

(defun canoDretPalo ()
    (angle (+ (/ (get 'cano 'amplada) 2) (get 'canoDret 'posicioInicial)) (+ (get 'cano 'alsada) (get 'campDret 'alsada)) 20 (get 'canoDret 'angle))
)

(defun canoEsquerra () 
    (rectangle (get 'canoEsquerra 'posicioInicial) (get 'campEsquerra 'alsada) (get 'cano 'amplada) (get 'cano 'alsada))
)

(defun canoEsquerraPalo ()
    (angle (+ (/ (get 'cano 'amplada) 2) (get 'canoEsquerra 'posicioInicial)) (+ (get 'cano 'alsada) (get 'campEsquerra 'alsada)) 20 (get 'canoEsquerra 'angle))
)

"GESTIO DELS XOCS"

; funcions que ens ajuden a saber si la posicio de la bala esta dins algun cano, o dins alguna part del mapa.
; Si la bala xoca a alguna part del mapa que no sigui un cano, llavors retornara 1, en el cas de que sigui el cano 
; esquerra retornara 3 i en el cas del cano dret 2. Per altra banda, si la bala no xoca en lloc retorna 0
(defun xocMapa (x y) (cond ((equal (xocCanoDret x y) 2) 2) ((equal (xocCanoEsquerra x y) 3) 3) ((equal (xocMur x y) 1) 1) ((equal (xocEnterra x y) 1) 1) ((equal (xocBordes x y) 1) 1) (t 0)))

(defun xocMur (x y) (cond ((and (> x (get 'mur 'posicioInicial)) (< x (+ (get 'mur 'posicioInicial) (get 'mur 'amplada))) (< y (get 'mur 'alsada))) 1) (t 0)))

(defun xocEnterra (x y) (cond ((or (and (> x (get 'campEsquerra 'iniciCamp)) (< x (get 'campEsquerra 'finalCamp)) (< y (get 'campEsquerra 'alsada))) (and (> x (get 'campDret 'iniciCamp)) (< x (get 'escenari 'amplada)) (< y (get 'campDret 'alsada)))) 1) (t 0)))

(defun xocBordes (x y) (cond ((or (< x (get 'campEsquerra 'iniciCamp)) (> x (get 'escenari 'amplada)) (> y (get 'escenari 'alsada))) 1) (t 0)))

(defun xocCanoDret (x y) (cond ((and (> x (get 'canoDret 'posicioInicial)) (< x (+ (get 'canoDret 'posicioInicial) (get 'cano 'amplada))) (> y (get 'campDret 'alsada)) (< y (+ (get 'campDret 'alsada) (get 'cano 'alsada)))) 2) (t 0)))

(defun xocCanoEsquerra (x y) (cond ((and (> x (get 'canoEsquerra 'posicioInicial)) (< x (+ (get 'canoEsquerra 'posicioInicial) (get 'cano 'amplada))) (> y (get 'campEsquerra 'alsada)) (< y (+ (get 'campEsquerra 'alsada) (get 'cano 'alsada)))) 3) (t 0)))

"GESTIÓ DELS GRAUS DELS CANONS"

(defun baixa-canoDret-graus () 
    (cond ((equal (get 'canoDret 'angle) 0) (putprop 'canoDret 1 'angle))
    ((equal (get 'canoDret 'angle) 180) (putprop 'canoDret 179 'angle))
    (t (putprop 'canoDret (+ (get 'canoDret 'angle) 1) 'angle)))
)

(defun puja-canoDret-graus ()
    (cond ((equal (get 'canoDret 'angle) 0) (putprop 'canoDret 1 'angle))
    ((equal (get 'canoDret 'angle) 180) (putprop 'canoDret 179 'angle))
    (t (putprop 'canoDret (- (get 'canoDret 'angle) 1) 'angle)))
)

(defun puja-canoEsquerra-graus () 
    (cond ((equal (get 'canoEsquerra 'angle) 0) (putprop 'canoEsquerra 1 'angle))
    ((equal (get 'canoEsquerra 'angle) 180) (putprop 'canoEsquerra 179 'angle))
    (t (putprop 'canoEsquerra (+ (get 'canoEsquerra 'angle) 1) 'angle)))
)

(defun baixa-canoEsquerra-graus ()
     (cond ((equal (get 'canoEsquerra 'angle) 0) (putprop 'canoEsquerra 1 'angle))
    ((equal (get 'canoEsquerra 'angle) 180)  (putprop 'canoEsquerra 179 'angle))
    (t (putprop 'canoEsquerra (- (get 'canoEsquerra 'angle) 1) 'angle)))
)

"GESTIÓ DE LA POSICIÓ DELS CANONS"

(defun mou-canoEsquerra-esquerra ()
  (let ((new-value (- (get 'canoEsquerra 'posicioInicial) 5)))
    (cond ((> new-value (round (get 'cano 'amplada)))(putprop 'canoEsquerra new-value 'posicioInicial))      
    )
  )
)

(defun mou-canoDret-esquerra ()
  (let ((new-value (- (get 'canoDret 'posicioInicial) 5)))
    (cond ((> new-value (+ (get 'campEsquerra 'finalCamp) (round (* (get 'cano 'amplada) 3)))) (putprop 'canoDret new-value 'posicioInicial))      
    )
  )
)

(defun mou-canoEsquerra-dreta ()
  (let ((new-value (+ (get 'canoEsquerra 'posicioInicial) 5)))
    (cond ((< new-value (- (get 'mur 'posicioInicial) (round (* (get 'cano 'amplada) 3)))) (putprop 'canoEsquerra new-value 'posicioInicial))      
    )
  )
)

(defun mou-canoDret-dreta ()
  (let ((new-value (+ (get 'canoDret 'posicioInicial) 5)))
    (cond ((< new-value (- (get 'escenari 'amplada) (round (* (get 'cano 'amplada) 2)))) (putprop 'canoDret new-value 'posicioInicial))          
    )
  )
)

"GESTIÓ DE LA POTÈNCIA DELS DISPARS"

(defun decr-potenciaDisparDret()
  (let ((new-value (- (get 'dispar 'potenciaDisparDret) 2)))
    (cond ((<= new-value 80) (putprop 'dispar (max new-value 20) 'potenciaDisparDret)))
  )
)
            
(defun incr-potenciaDisparDret()
  (let ((new-value (+ (get 'dispar 'potenciaDisparDret) 2)))
    (cond ((<= new-value 80) (putprop 'dispar (max new-value 20) 'potenciaDisparDret)))
  )
)

(defun decr-potenciaDisparEsquerra()
  (let ((new-value (- (get 'dispar 'potenciaDisparEsquerra) 2)))
    (cond ((<= new-value 80) (putprop 'dispar (max new-value 20) 'potenciaDisparEsquerra)))
  )
)
            
(defun incr-potenciaDisparEsquerra()
  (let ((new-value (+ (get 'dispar 'potenciaDisparEsquerra) 2)))
    (cond ((<= new-value 80) (putprop 'dispar (max new-value 20) 'potenciaDisparEsquerra)))
  )
)

"PINTAR LA INTERFÍCIE"

(defun pinta ()
    (cls)    
    (escenari)
    (campEsquerra)
    (campDret)    
    (mur)
    (bandera)
    (canoDret)         
    (canoEsquerra)
    (canoEsquerraPalo)
    (canoDretPalo) 
)

(defun pinta-color ()
    (cls)
    (color 0 0 0)  
    (escenari)
    (color 0 255 0)
    (campEsquerra)
    (color 0 0 255)
    (campDret)
    (color 255 0 0)    
    (mur)
    (color 255 0 255)
    (bandera)
    (color 40 90 90)
    (canoDret)
    (color 0 255 255)         
    (canoEsquerra)
    (color 100 50 175)
    (canoEsquerraPalo)
    (canoDretPalo) 
)

"FUNCIONS ADDICIONALS"

(defun drawr (x y)
  (draw (round x) 
        (round y))
)

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
)

; igual que la funció per dibuixar el rectangle però en funció de la n divideix la bandera. Es crida recursivament
(defun trocosBandera (x y w h n)
  (cond ((= n 0) nil)
        (t (let* ((step-x (round (/ w n)))
                  (step-y (round h)))
             (move (round x) (round y))
             (drawrel step-x 0)
             (drawrel 0 step-y)
             (drawrel (- step-x) 0)
             (drawrel 0 (- step-y))
             (trocosBandera (+ x step-x) y (- w step-x) h (- n 1)))))
)

(defun angle (x y r angle)
    "Dibuix palo canó"
    (move x y)
    (drawr (+ x (* r (cos (radians angle))))
           (+ y (* r (sin (radians angle)))))
)

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360)
)

(defun obtenir-valor-aleatori (min max)
  (+ min (random (+ 1 (- max min))))
)

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime)))
)

(defun repeteix (valor)
    (cond 
          ((equal (get 'xoc 'valorXoc) 2)
            (color 0 0 0)
            (cls)
            (princ "CANO ESQUERRA GUANYA") t)
          ((equal (get 'xoc 'valorXoc) 3)
            (color 0 0 0)
            (cls)
            (princ "CANO DRET GUANYA") t) 
          ((equal valor 0) (pinta)
              (color 0 0 0)    
              (princ "MOURE CANONS: A-D,J-L\t")
              (princ "ANGLE CANONS: W-S,I-K\t")
              (princ "POTENCIA DISPAR: E-Q,U-O\t")
              (princ "DISPARAR: F-H\t")
              (princ "POT ESQ: ")
              (princ (get 'dispar 'potenciaDisparEsquerra))
              (princ "\t")
              (princ "POT DRETA: ")
              (princ (get 'dispar 'potenciaDisparDret)))
          (t (pinta-color)
              (color 0 0 0)    
              (princ "MOURE CANONS: A-D,J-L\t")
              (princ "ANGLE CANONS: W-S,I-K\t")
              (princ "POTENCIA DISPAR: E-Q,U-O\t")
              (princ "DISPARAR: F-H\t")
              (princ "POT ESQ: ")
              (princ (get 'dispar 'potenciaDisparEsquerra))
              (princ "\t")
              (princ "POT DRETA: ")
              (princ (get 'dispar 'potenciaDisparDret)) 
          )
    )    
    (setq tecla (get-key))
    (cond           
          ((equal (get 'xoc 'valorXoc) 2) t)    
          ((equal (get 'xoc 'valorXoc) 3) t)                
          ((equal tecla 97) ; a
            (mou-canoEsquerra-esquerra) (repeteix valor)) ; mou cap a l'esquerra canó esquerra
          ((equal tecla 106) ; j
            (mou-canoDret-esquerra) (repeteix valor)) ; mou cap a l'esquerra canó dret
          ((equal tecla 100) ; d
            (mou-canoEsquerra-dreta) (repeteix valor)) ; mou cap a la dreta canó esquerra
          ((equal tecla 108) ; l
            (mou-canoDret-dreta) (repeteix valor)) ; mou cap a la dreta canó dret
          ((equal tecla 119) ; w
            (puja-canoEsquerra-graus) (repeteix valor)) ; puja canó esquerra
          ((equal tecla 105) ; i
            (puja-canoDret-graus) (repeteix valor)) ; puja canó dret
          ((equal tecla 115) ; s
            (baixa-canoEsquerra-graus) (repeteix valor)) ; baixa canó esquerra
          ((equal tecla 107) ; k
            (baixa-canoDret-graus) (repeteix valor)) ; baixa canó dret
          ((equal tecla 113) ; q
            (decr-potenciaDisparEsquerra) (repeteix valor)) ; menys potència canó esquerra
          ((equal tecla 111) ; o
            (decr-potenciaDisparDret) (repeteix valor)) ; menys potència canó dret
          ((equal tecla 101) ; e
            (incr-potenciaDisparEsquerra) (repeteix valor)) ; més potència canó esquerra
          ((equal tecla 117) ; u
            (incr-potenciaDisparDret) (repeteix valor)) ; més potència canó dret
          ((equal tecla 102) ; f        
            (move (round (+ (+ (/ (get 'cano 'amplada) 2) (get 'canoEsquerra 'posicioInicial)) (* 20 (cos (radians (get 'canoEsquerra 'angle)))))) (round (+ (+ (get 'cano 'alsada) (get 'campEsquerra 'alsada)) (* 20 (sin (radians (get 'canoEsquerra 'angle)))))))    
            (viX (get 'canoEsquerra 'angle) (get 'dispar 'potenciaDisparEsquerra))
            (viY (get 'canoEsquerra 'angle) (get 'dispar 'potenciaDisparEsquerra))
            (calculTemps)
            (calculRang)
            (trajectoria (+ (+ (/ (get 'cano 'amplada) 2) (get 'canoEsquerra 'posicioInicial)) (* 20 (cos (radians (get 'canoEsquerra 'angle))))) (+ (+ (get 'cano 'alsada) (get 'campEsquerra 'alsada)) (* 20 (sin (radians (get 'canoEsquerra 'angle))))) 0 (vent))
            (repeteix valor)) ; dispara canó esquerra
          ((equal tecla 104) ; h
            (move (round (+ (+ (/ (get 'cano 'amplada) 2) (get 'canoDret 'posicioInicial)) (* 20 (cos (radians (get 'canoDret 'angle)))))) (round (+ (+ (get 'cano 'alsada) (get 'campDret 'alsada)) (* 20 (sin (radians (get 'canoDret 'angle)))))))
            (viX (get 'canoDret 'angle) (get 'dispar 'potenciaDisparDret))
            (viY (get 'canoDret 'angle) (get 'dispar 'potenciaDisparDret))
            (calculTemps)
            (calculRang)
            (trajectoria (+ (+ (/ (get 'cano 'amplada) 2) (get 'canoDret 'posicioInicial)) (* 20 (cos (radians (get 'canoDret 'angle))))) (+ (+ (get 'cano 'alsada) (get 'campDret 'alsada)) (* 20 (sin (radians (get 'canoDret 'angle))))) 0 (vent))
            (repeteix valor)) ; dispara canó dret
          ((equal tecla 27)  ; ESC
           (color 0 0 0)
           (cls) t) ; acaba recursió
          (t ; altrament
           (repeteix valor))) ; repeteix
)