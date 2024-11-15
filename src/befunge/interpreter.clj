;; Aca va a ir la lógica principal para interpretar los comandos Befunge-93
(ns befunge.interpreter
  (:require [befunge.stack :as stack]
            [befunge.torus :as torus]))

(def direcciones
  {:derecha [0 1], :izquierda [0 -1], :arriba [-1 0], :abajo [1 0]})

(defn crear-entorno
  "Inicializa el entorno del programa Befunge-93."
  [programa]
  {:programa programa
   :posicion [0 0]  ;; Posición inicial en el programa
   :direccion (direcciones :derecha)  ;; Dirección inicial
   :modo-cadena false
   :terminado false})

(defn mover
  "Mueve el puntero en la dirección indicada, con envolvimiento de toroide."
  [{:keys [posicion direccion programa] :as entorno}]
  (let [[x y] posicion
        [dx dy] direccion]  ;; Desempaqueta la dirección en sus componentes
    (if (empty? programa)
      entorno
      ;; Calcula la nueva posición sumando las componentes de dirección
      (let [nueva-posicion [(mod (+ x dx) (count programa))  ;; Envolvimiento en X
                            (mod (+ y dy) (count (first programa)))]]  ;; Envolvimiento en Y
        (assoc entorno :posicion nueva-posicion)))))  ;; Actualiza la posición en el entorno

(defn cambiar-direccion
  "Cambia la dirección en el entorno según el comando."
  [entorno nueva-direccion]
  (let [nueva-direccion-vector (get direcciones nueva-direccion)]
    (if nueva-direccion-vector
      (assoc entorno :direccion nueva-direccion-vector)
      entorno)))  ;; Si no es válida, no cambia la dirección


(defn obtener-comando
  "Obtiene el comando en la posición actual del toroide, reemplazando valores nulos por espacios."
  [programa [x y]]
  (let [comando (get-in programa [x y])]
    (if comando
      comando
      \space))) ;; Usamos espacio si la celda está vacía

(defn interpretar-comando
  "Interpreta el comando actual y realiza la operación correspondiente."
  [{:keys [programa posicion direccion modo-cadena] :as entorno}]
  (let [comando (obtener-comando programa posicion)]

    ;;(println "Comando actual:" comando "en posición:" posicion "Dirección:" direccion)  ;; ----------------------------Depuración---------------------------

    (cond

      ;; Si el modo string esta activo, los caracteres van directo a la pila
      modo-cadena
      (if (= comando \")
        (do
          ;; (println "Saliendo del modo cadena.")             ;; ----------------------------Depuración---------------------------
          (assoc entorno :modo-cadena false))
        (do
          (stack/apilar (int comando))
          entorno))

      ;; Comando de terminación
      (= comando \@)
      (do
        ;;(println "Programa terminado.")               ;; ----------------------------Depuración---------------------------
        (assoc entorno :terminado true))

      ;; Comando de apilar dígito
      (and (not modo-cadena) (Character/isDigit comando))
      (do
        (stack/apilar (Character/digit comando 10))
        
         ;;(println "    Pila después de apilar:" (stack/obtener-pila)) ;; ----------------------------Depuración---------------------------

        entorno)

      ;; Comandos de dirección
      (= comando \>) (cambiar-direccion entorno :derecha)
      (= comando \<) (cambiar-direccion entorno :izquierda)
      (= comando \^) (cambiar-direccion entorno :arriba)
      (= comando \v) (cambiar-direccion entorno :abajo)
      (= comando \?) (let [direccion-aleatoria (rand-nth [:derecha :izquierda :arriba :abajo])]
                      ;(println "Cambio de direccion a: " direccion-aleatoria)    ;; ----------------------------Depuración---------------------------
                      (cambiar-direccion entorno direccion-aleatoria))

      ;; Comandos logicos de dirección
      (= comando \_) (do
                       (let [bool (stack/desapilar)]
                         (if (not (zero? bool)) (cambiar-direccion entorno :izquierda) (cambiar-direccion entorno :derecha))))
      (= comando \|) (do
                       (let [bool (stack/desapilar)]
                         (if (not (zero? bool)) (cambiar-direccion entorno :arriba) (cambiar-direccion entorno :abajo))))

      ;; Comandos logicos
      (= comando \!) (do (stack/negacion-logica) entorno)
      (= comando \`) (do (stack/mayor-que) entorno)

      ;; Comando de modo cadena
      (= comando \")(do

                      ;; (println "Cambiando modo cadena a" (not modo-cadena))       ;; ----------------------------Depuración---------------------------

                      (assoc entorno :modo-cadena (not modo-cadena)))

      ;; Comandos de I/O
      (= comando \.) (let [a (stack/desapilar)]
                       (print (int a )" ") entorno)            
      (= comando \,) (let [a (stack/desapilar)]
                       (print(char a ))
                       entorno)
      (= comando \&) (do
                       (println "Ingrese un entero: ")                   
                       (let [a (Integer/parseInt (read-line))]
                         (stack/apilar a)) entorno)
      (= comando \~) (do
                       (println "Ingrese un caracter: ")        
                       (let [a (char (read-line))]
                         (stack/apilar a)) entorno)

      ;; Comandos de pila directa
      (= comando \:) (do (stack/duplicar) entorno)
      (= comando \\) (do (stack/intercambiar) entorno)
      (= comando \$) (do (stack/descartar) entorno)

      ;; Comandos de aritmetica
      (= comando \+) (do (stack/sumar) entorno)
      (= comando \-) (do (stack/restar) entorno)
      (= comando \*) (do (stack/multiplicar) entorno)
      (= comando \/) (do (stack/dividir) entorno)
      (= comando \%) (do (stack/modulo) entorno)

      ;; Comando de movimiento
      (= comando \#) (let [nuevo-entorno (mover entorno)]
                      ;; (println "Skipeando una posicion")   ;; ----------------------------Depuración---------------------------
                      nuevo-entorno)

      ;; Comandos de toroide
      (= comando \g) (do
                       (let [a (stack/desapilar)
                             b (stack/desapilar)]
                         (stack/apilar (torus/obtener b a))) entorno)
      (= comando \p) (do
                       (let [a (stack/desapilar)
                             b (stack/desapilar)
                             c (stack/desapilar)]
                         (torus/establecer b a c)) entorno)

      ;; Otros comandos
      :else
       (do
        ;; Si el comando no es especial, simplemente mueve a la siguiente celda

        ;;(println "Comando no especial, moviendo puntero.")    ;; ----------------------------Depuración---------------------------
        
        entorno))))

(defn ejecutar-programa
  "Ejecuta el programa Befunge-93 en el entorno dado, procesando celda por celda."
  [programa]
  (let [entorno (crear-entorno programa)]  ;; Asume que 'crear-entorno' inicializa el entorno correctamente.
    (loop [env entorno]
      ;; (println "Estado actual:" env)  ;; ----------------------------Depuración---------------------------
      (if (:terminado env)  ;; Si el programa terminó
        nil
        (let [comando (obtener-comando programa (:posicion env))]

          ;;(println "Comando actual:" comando)     ;; ----------------------------Depuración---------------------------

          (recur (mover (interpretar-comando env))))))))  ;; Recur correctamente, pasando el entorno actualizado
