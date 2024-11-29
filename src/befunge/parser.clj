(ns befunge.parser
  (:require [clojure.java.io :as io]))

(defn cargar-programa
  "Carga un programa Befunge-93 desde un archivo y lo convierte en un toroide.
   Las líneas se rellenan con espacios para igualar el ancho máximo."
  [ruta]
  (try
    (with-open [lector (io/reader ruta)]
      (let [lineas (doall (line-seq lector))        
            ancho-maximo (if (seq lineas)                   
                        (apply max (map count lineas))
                        0)                              
            programa (vec (map #(vec (concat % (repeat (- ancho-maximo (count %)) \space))) 
                               lineas))]                 
        programa))                                       
    (catch Exception e
      (println "Error al cargar el archivo:" (.getMessage e))
      nil)))                                            
