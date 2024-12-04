(ns befunge.parser
  (:require [clojure.java.io :as io]))

(defn cargar-programa [ruta]
  (try
    (with-open [lector (io/reader ruta)]
      (let [lineas (doall (line-seq lector))
            ancho-maximo (apply max 0 (map count lineas))]
        (vec (map #(vec (concat % (repeat (- ancho-maximo (count %)) \space))) lineas))))
    (catch Exception e
      (println "Error al cargar el archivo:" (.getMessage e))
      nil)))
