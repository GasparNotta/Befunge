(ns befunge.interpreter
  (:require [clojure.java.io :as io]
            [befunge.stack :as stack]))

;; Direcciones
(def direcciones
  {:derecha [0 1], :izquierda [0 -1], :arriba [-1 0], :abajo [1 0]})

;; Estado inicial del intérprete
(defn crear-entorno
  "Inicializa el entorno del programa Befunge-93."
  [programa]
  {:programa programa
   :posicion [0 0]
   :direccion :derecha
   :modo-cadena false
   :pila []})

(defn mover
  "Mueve el puntero en la dirección indicada, con envolvimiento de toroide."
  [{:keys [posicion direccion programa] :as entorno}]
  (let [[x y] posicion
        [dx dy] (direcciones direccion)
        bordear (fn [valor max] (mod (+ valor max) max))]
    (-> entorno
        (update :posicion
                #(vector (bordear (+ (first %) dx) (count programa))
                         (bordear (+ (second %) dy) (count (first programa))))))))

(defn interpretar-comando
  "Interpreta el comando actual y realiza la operación correspondiente."
  [entorno comando]
  (let [pila (:pila entorno)]
  (if (:modo-cadena entorno)
    ;; Modo cadena: agrega el valor ASCII al stack
    (if (= comando \")
      (update entorno :modo-cadena not)
      (let [pila1 (stack/apilar pila (int comando))]
        (assoc entorno :pila pila1)))
    
    (case comando
      ;; Comandos de dirección
      \> (assoc entorno :direccion :derecha)
      \< (assoc entorno :direccion :izquierda)
      \^ (assoc entorno :direccion :arriba)
      \v (assoc entorno :direccion :abajo)
      \? (assoc entorno :direccion (rand-nth (keys direcciones)))

      ;; Comandos lógicos de dirección
      \_ (let [[bool pila1] (stack/desapilar pila)]
           (-> entorno
               (assoc :direccion (if (zero? bool) :derecha :izquierda))
               (assoc :pila pila1)))

      \| (let [[bool pila1] (stack/desapilar pila)]
           (-> entorno
               (assoc :direccion (if (zero? bool) :abajo :arriba))
               (assoc :pila pila1)))

      ;; Comandos lógicos
      \! (do (assoc entorno :pila (stack/negacion-logica pila)))
      \` (do (assoc entorno :pila (stack/mayor-que pila)))
      
      ;; Comando de modo cadena
      \" (update entorno :modo-cadena not)

      ;; Comandos de I/O
      \. (let [[num pila1] (stack/desapilar pila)]
           (when num (print (str num " ")))
           (assoc entorno :pila pila1))
      \, (let [[chr pila1] (stack/desapilar pila)]
           (when chr (print (char chr)))
           (assoc entorno :pila pila1))
      \& (do
           (let [a (Integer/parseInt (read-line))]
             (assoc entorno :pila (stack/apilar pila a))))
      \~ (do
           (let [a (int (first (read-line)))]
             (assoc entorno :pila (stack/apilar pila a))))

      ;; Comandos de pila directa
      \: (do (assoc entorno :pila (stack/duplicar pila)))
      \\ (do (assoc entorno :pila (stack/intercambiar pila)))
      \$ (do (assoc entorno :pila (stack/descartar pila)))

      ;; Comandos aritméticos
      \+ (do (assoc entorno :pila (stack/sumar pila)))
      \- (do (assoc entorno :pila (stack/restar pila)))
      \* (do (assoc entorno :pila (stack/multiplicar pila)))
      \/ (do (assoc entorno :pila (stack/dividir pila)))
      \% (do (assoc entorno :pila (stack/modulo pila)))

      ;; Comando de movimiento
      \# (mover entorno)

      ;; Comandos de toroide
      \p (let [[a pila1] (stack/desapilar pila)
               [b pila2] (stack/desapilar pila1)
               [c pila3] (stack/desapilar pila2)
               programa1 (assoc-in (:programa entorno) [a b] (char c))]
           (assoc entorno :programa programa1 :pila pila3))

      \g (let [[a pila1] (stack/desapilar pila)
               [b pila2] (stack/desapilar pila1)]
           (if (and (< a (count (:programa entorno)))
                    (< b (count (first (:programa entorno)))))
          (assoc entorno :pila (stack/apilar pila2 (int (get-in (:programa entorno) [a b]))))
          (assoc entorno :pila (stack/apilar pila2 0))))

      (if (Character/isDigit comando)
        (let [pila1 (stack/apilar pila (Character/digit comando 10))]
          (assoc entorno :pila pila1))
        entorno)))))

(defn ejecutar-programa
  "Ejecuta el programa Befunge-93 desde el archivo."
  [programa]
  (loop [entorno (crear-entorno programa)]
    (let [comando (get-in (:programa entorno) (:posicion entorno))]
      (if (= comando \@)
        nil
        (recur (-> entorno
                  (interpretar-comando comando)  ;; Interpretar el comando
                  (mover)))))))

