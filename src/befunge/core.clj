(ns befunge.core
  (:require [befunge.interpreter :as interpreter]
            [befunge.parser :as parser]))

(defn -main [& args]
  (if-let [ruta-archivo (first args)]
    (some-> ruta-archivo parser/cargar-programa interpreter/ejecutar-programa)
    (println "Por favor, proporciona la ruta de un archivo Befunge-93 para ejecutar.")))
