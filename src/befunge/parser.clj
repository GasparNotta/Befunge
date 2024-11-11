;; Aca va a ir las funciones para la carga de programas Befunge-93
(ns befunge.parser
  (:require [befunge.torus :as torus]))

;; Función para cargar un programa Befunge-93 desde un archivo y convertirlo en un toroide.
(defn cargar-programa
  "Carga un programa Befunge-93 desde un archivo y lo convierte en un toroide."
  [ruta]
  (let [lineas (slurp ruta)  ;; Lee el archivo como un solo string
        programa (mapv #(vec %) (clojure.string/split-lines lineas))]  ;; Convierte a una lista de listas
    (torus/inicializar-toroide programa)
    (torus/mostrar-toroide)  ;; Depuración
    programa))
