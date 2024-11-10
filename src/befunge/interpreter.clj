;; Aca va a ir la lógica principal para interpretar los comandos Befunge-93

(ns befunge.interpreter
  (:require [befunge.stack :as stack]
            [befunge.parser :as parser]
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
    ;; Calcula la nueva posición sumando las componentes de dirección
    (let [nueva-posicion [(mod (+ x dx) (count programa))  ;; Envolvimiento en X
                          (mod (+ y dy) (count (first programa)))]]  ;; Envolvimiento en Y
      (println "Moviendo de" posicion "a" nueva-posicion)  ;; Depuración
      (assoc entorno :posicion nueva-posicion))))  ;; Actualiza la posición en el entorno

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
  (let [[x y] posicion
        comando (obtener-comando programa posicion)]
    (println "Comando actual:" comando "en posición:" posicion "Dirección:" direccion)
    (cond
      ;; Comando de terminación
      (= comando \@)
      (do
        (println "Programa terminado.")
        (assoc entorno :terminado true))

      ;; Comando de apilar dígito
      (and (not modo-cadena) (Character/isDigit comando))
      (do
        (stack/apilar (Character/digit comando 10))
        (println "Pila después de apilar:" (stack/obtener-pila))
        entorno)

      ;; Comandos de dirección
      (= comando \>) (cambiar-direccion entorno :derecha)
      (= comando \<) (cambiar-direccion entorno :izquierda)
      (= comando \^) (cambiar-direccion entorno :arriba)
      (= comando \v) (cambiar-direccion entorno :abajo)
      (= comando \?) ()

      ;; Comandos logicos de dirección
      (= comando \_) (do
                       (let [bool (stack/desapilar)]
                         (if bool (cambiar-direccion entorno :izquierda) (cambiar-direccion entorno :derecha))))
      (= comando \|) (do
                       (let [bool (stack/desapilar)]
                         (if bool (cambiar-direccion entorno :arriba) (cambiar-direccion entorno :abajo))))

      ;; Comandos logicos
      (= comando \!) (stack/negacion-logica)
      (= comando \`) (stack/mayor-que)

      ;; Comando de modo cadena
      (= comando \") (do
                      (println "Cambiando modo cadena a" (not modo-cadena))
                      (assoc entorno :modo-cadena (not modo-cadena)))

      ;; Comandos de I/O
      (= comando \.) (let [a (stack/desapilar)]
                       (print (int a)))
      (= comando \,) (let [a (stack/desapilar)]
                       (print (char a)))
      (= comando \&) (do
                       (println "Ingrese un entero: ")
                       (let [a (Integer/parseInt (read-line))]
                         (stack/apilar a)))
      (= comando \~) (do
                       (println "Ingrese un caracter: ")
                       (let [a (Character/toChars (read-line))]
                         (stack/apilar a)))

      ;; Comandos de pila directa
      (= comando \:) (stack/duplicar)
      (= comando \\) (stack/intercambiar)
      (= comando \$) (stack/descartar)

      ;; Comandos de aritmetica
      (= comando \+) (stack/sumar)
      (= comando \-) (stack/restar)
      (= comando \*) (stack/multiplicar)
      (= comando \/) (stack/dividir)
      (= comando \%) (stack/modulo)

      ;; Comando de movimiento
      (= comando \#) ()

      ;; Comandos de toroide
      (= comando \g) (do
                       (let [a (stack/desapilar)
                             b (stack/desapilar)]
                         (stack/apilar (torus/obtener b a))))
      (= comando \p) (do
                       (let [a (stack/desapilar)
                             b (stack/desapilar)
                             c (stack/desapilar)]
                         (stack/apilar (torus/establecer b a c))))

      ;; Otros comandos
      :else
       (do
        ;; Si el comando no es especial, simplemente mueve a la siguiente celda
        (println "Comando no especial, moviendo puntero.")
        entorno))))

(defn ejecutar-programa
  "Ejecuta el programa Befunge-93 en el entorno dado, procesando celda por celda."
  [programa]
  (let [entorno (crear-entorno programa)]  ;; Asume que 'crear-entorno' inicializa el entorno correctamente.
    (loop [env entorno]
      (println "Estado actual:" env)  ;; Depuración
      (if (:terminado env)  ;; Si el programa terminó
        (println "Programa terminado.")
        (let [comando (obtener-comando programa (:posicion env))]
          (println "Comando actual:" comando)
          (recur (mover (interpretar-comando env))))))))  ;; Recur correctamente, pasando el entorno actualizado

(defn cargar-programa
  "Carga un programa Befunge-93 desde un archivo y lo convierte en un toroide."
  [ruta]
  (let [lineas (slurp ruta)  ;; Lee el archivo como un solo string
        programa (mapv #(vec %) (clojure.string/split-lines lineas))]  ;; Convierte a una lista de listas (toroide)
    programa))  ;; Retorna el toroide
    ;; Ejemplo de uso
    (defn -main []
      (let [programa (cargar-programa "/path/to/your/befunge/program.bf")]
        (ejecutar-programa programa)))
    
    ;; Para ejecutar el programa, descomenta la siguiente línea y ajusta la ruta del archivo
    ;; (-main)