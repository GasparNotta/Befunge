;; Aca van a ir las funciones para manipular la pila
(ns befunge.parser
  (:require [befunge.torus :as torus]))

(defn cargar-programa
  "Carga un programa Befunge-93 desde un archivo y lo convierte a un toroide."
  [ruta]
  (let [contenido (slurp ruta)]
    (torus/inicializar-toroide (clojure.string/split-lines contenido))))
