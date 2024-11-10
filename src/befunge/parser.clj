;; Aca va a ir la lógica para leer y representar el código Befunge-93

(ns befunge.parser
  "Módulo para manejar la carga y parsing del archivo del programa Befunge-93.")

(require '[clojure.java.io :as io])

(defn leer-archivo
  "Lee un archivo de texto y devuelve una lista de líneas de texto.
   Cada línea es una secuencia de caracteres que representa una fila del programa Befunge-93."
  [ruta-archivo]
  (with-open [lector (io/reader ruta-archivo)]
    (doall (line-seq lector))))

(defn cargar-programa
  "Carga el programa Befunge-93 desde un archivo especificado y lo convierte a una lista de líneas.
   Utiliza esta lista para inicializar el toroide."
  [ruta-archivo]
  (let [lineas (leer-archivo ruta-archivo)]
    (mapv #(vec %) lineas))) ; Convierte cada línea en un vector de caracteres para mayor manipulación
