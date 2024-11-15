;; Aca va a ir el punto de entrada y la función `-main`
(ns befunge.core
  (:require [befunge.interpreter :as interpreter]
            [befunge.parser :as parser]))

(defn -main
  "Función principal que ejecuta el intérprete de Befunge-93."
  [& args]
  (if (empty? args)
    (println "Por favor, proporciona la ruta de un archivo Befunge-93 para ejecutar.")
    (let [ruta-archivo (first args)
          programa (parser/cargar-programa ruta-archivo)]
      (interpreter/ejecutar-programa programa))))