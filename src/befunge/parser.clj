(ns befunge.parser
  (:require [befunge.torus :as torus]
            [clojure.string :as str]))

(defn cargar-programa
  "Carga un programa Befunge-93 desde un archivo y lo convierte en un toroide."
  [ruta]
  (try
    (let [lineas (str/split-lines (slurp ruta))  ;; Lee el archivo y divide por líneas
          programa (mapv #(vec %) lineas)]  ;; Convierte cada línea en un vector de caracteres
      (torus/inicializar-toroide programa)
      programa)  ;; Retorna el programa si todo sale bien
    (catch Exception e
      (println "Error al cargar el archivo:" (.getMessage e))
      nil)))  ;; Si hay error, devuelve nil
