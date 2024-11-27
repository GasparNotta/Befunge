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
        [dx dy] direccion]
    (if (empty? programa)
      entorno
      (let [nueva-posicion [(mod (+ x dx) (count programa))
                            (mod (+ y dy) (count (first programa)))]]
        (assoc entorno :posicion nueva-posicion)))))

(defn cambiar-direccion
  "Cambia la dirección en el entorno según el comando."
  [entorno nueva-direccion]
  (if-let [nueva-direccion-vector (get direcciones nueva-direccion)]
    (assoc entorno :direccion nueva-direccion-vector)
    entorno))

(defn obtener-comando
  "Obtiene el comando en la posición actual del toroide, reemplazando valores nulos por espacios."
  [programa [x y]]
  (get-in programa [x y] \space))

(defn interpretar-comando
  "Interpreta el comando actual y realiza la operación correspondiente."
  [{:keys [programa posicion direccion modo-cadena] :as entorno}]
  (let [comando (obtener-comando programa posicion)]
    (cond
      ;; Modo cadena: agrega el valor ASCII al stack
      modo-cadena
      (if (= comando \")
        (assoc entorno :modo-cadena false)
        (do
          (stack/apilar (int comando))
          entorno))

      ;; Comando de terminación
      (= comando \@)
      (assoc entorno :terminado true)

      ;; Comando de apilar dígito
      (and (not modo-cadena) (Character/isDigit comando))
      (do
        (stack/apilar (Character/digit comando 10))
        entorno)

      ;; Comandos de dirección
      (= comando \>) (cambiar-direccion entorno :derecha)
      (= comando \<) (cambiar-direccion entorno :izquierda)
      (= comando \^) (cambiar-direccion entorno :arriba)
      (= comando \v) (cambiar-direccion entorno :abajo)
      (= comando \?) (let [direccion-aleatoria (rand-nth [:derecha :izquierda :arriba :abajo])]
                      (cambiar-direccion entorno direccion-aleatoria))

      ;; Comandos lógicos de dirección
      (= comando \_) (let [bool (stack/desapilar)]
                       (cambiar-direccion entorno (if (zero? bool) :derecha :izquierda)))
      (= comando \|) (let [bool (stack/desapilar)]
                       (cambiar-direccion entorno (if (zero? bool) :abajo :arriba)))

      ;; Comandos lógicos
      (= comando \!) (do (stack/negacion-logica) entorno)
      (= comando \`) (do (stack/mayor-que) entorno)

      ;; Comando de modo cadena
      (= comando \") (assoc entorno :modo-cadena (not modo-cadena))

      ;; Comandos de I/O
      (= comando \.) (do (print (int (stack/desapilar))"") entorno)
      (= comando \,) (do (print (char (stack/desapilar))) entorno)
      (= comando \&) (do
                       (let [a (Integer/parseInt (read-line))]
                         (stack/apilar a))
                       entorno)
      (= comando \~) (do
                       (let [a (int (first (read-line))) ]
                         (stack/apilar a))
                       entorno)

      ;; Comandos de pila directa
      (= comando \:) (do (stack/duplicar) entorno)
      (= comando \\) (do (stack/intercambiar) entorno)
      (= comando \$) (do (stack/descartar) entorno)

      ;; Comandos aritméticos
      (= comando \+) (do (stack/sumar) entorno)
      (= comando \-) (do (stack/restar) entorno)
      (= comando \*) (do (stack/multiplicar) entorno)
      (= comando \/) (do (stack/dividir) entorno)
      (= comando \%) (do (stack/modulo) entorno)

      ;; Comando de movimiento
      (= comando \#) (mover entorno)

      ;; Comandos de toroide
      (= comando \g) (let [a (stack/desapilar)
                           b (stack/desapilar)]
                       (stack/apilar (torus/obtener b a))
                       entorno)
      (= comando \p) (let [a (stack/desapilar)
                           b (stack/desapilar)
                           c (stack/desapilar)]
                       (torus/establecer b a c)
                       entorno)

      ;; Otros comandos
      :else
      entorno)))

(defn ejecutar-programa
  "Ejecuta el programa Befunge-93 en el entorno dado, procesando celda por celda."
  [programa]
  (let [entorno (crear-entorno programa)]
    (loop [env entorno]
      (if (:terminado env)
        nil
        (recur (mover (interpretar-comando env)))))))
