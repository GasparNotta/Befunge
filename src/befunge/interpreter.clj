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
   :modo-cadena false})

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
  (if (:modo-cadena entorno)
    ;; Modo cadena: agrega el valor ASCII al stack
    (if (= comando \")
      (update entorno :modo-cadena not)
      (do (stack/apilar (int comando)) entorno))
    
    (case comando
      ;; Comandos de dirección
      \> (assoc entorno :direccion :derecha)
      \< (assoc entorno :direccion :izquierda)
      \^ (assoc entorno :direccion :arriba)
      \v (assoc entorno :direccion :abajo)
      \? (assoc entorno :direccion (rand-nth (keys direcciones)))

      ;; Comandos lógicos de dirección
      \_ (let [bool (stack/desapilar)]
           (assoc entorno :direccion (if (zero? bool) :derecha :izquierda)))
      \| (let [bool (stack/desapilar)]
           (assoc entorno :direccion (if (zero? bool) :abajo :arriba)))

      ;; Comandos lógicos
      \! (do (stack/negacion-logica) entorno)
      \` (do (stack/mayor-que) entorno)
      
      ;; Comando de modo cadena
      \" (update entorno :modo-cadena not)

      ;; Comandos de I/O
      \. (let [num (stack/desapilar)]
           (when num (print (str num " ")))
           entorno)
      \, (let [chr (stack/desapilar)]
           (when chr (print (char chr)))
           entorno)
      \& (do
           (let [a (Integer/parseInt (read-line))]
             (stack/apilar a))
           entorno)
      \~ (do
           (let [a (int (first (read-line)))]
             (stack/apilar a))
           entorno)

      ;; Comandos de pila directa
      \: (do (stack/duplicar) entorno)
      \\ (do (stack/intercambiar) entorno)
      \$ (do (stack/descartar) entorno)

      ;; Comandos aritméticos
      \+ (do (stack/sumar) entorno)
      \- (do (stack/restar) entorno)
      \* (do (stack/multiplicar) entorno)
      \/ (do (stack/dividir) entorno)
      \% (do (stack/modulo) entorno)

      ;; Comando de movimiento
      \# (mover entorno)

      ;; Comandos de toroide
      \p (let [a (stack/desapilar)
               b (stack/desapilar)
               c (stack/desapilar)]
           (update entorno :programa update-in [a b] (constantly (char c))))
      \g (let [a (stack/desapilar)
               b (stack/desapilar)]
           (if (and (< a (count (:programa entorno)))
                    (< b (count (first (:programa entorno)))))
             (do (stack/apilar (int (get-in (:programa entorno) [a b])))
                 entorno))) 
                    
      (if (Character/isDigit comando)
        (do (stack/apilar (Character/digit comando 10)) entorno)
        entorno))))

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

