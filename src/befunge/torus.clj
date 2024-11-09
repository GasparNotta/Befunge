;; Aca van a ir la lógica de manejo del toroide de 80x25 caracteres

(ns befunge.torus
  "Módulo para manejar el toroide de 80x25 caracteres en el intérprete de Befunge-93.")

;; Dimensiones del toroide
(def ancho 80)
(def alto 25)

;; Representación del toroide como un mapa de coordenadas (x, y) a valores ASCII
(def torus (atom {}))

;; Función para inicializar el toroide con el programa desde una lista de líneas.
(defn inicializar-toroide
  "Carga el programa en el toroide a partir de una lista de líneas de texto.
   Cada carácter en el texto se coloca en la posición correspondiente del toroide."
  [lineas]
  (reset! torus {})
  (doseq [y (range (count lineas))
          :let [linea (nth lineas y)]]
    (doseq [x (range (count linea))]
      (swap! torus assoc [x y] (int (nth linea x))))))

;; Función para obtener el valor en una posición (x, y) en el toroide.
(defn obtener
  "Devuelve el valor ASCII en la posición (x, y) del toroide.
   Si no hay un valor definido, devuelve el valor ASCII de un espacio (32)."
  [x y]
  (get @torus [(mod x ancho) (mod y alto)] 32))

;; Función para establecer un valor en una posición (x, y) en el toroide.
(defn establecer
  "Coloca un valor ASCII en la posición (x, y) del toroide."
  [x y valor]
  (swap! torus assoc [(mod x ancho) (mod y alto)] valor))

;; Función para mostrar el contenido actual del toroide, útil para depuración.
(defn mostrar-toroide
  "Devuelve una representación del toroide para depuración, donde cada celda vacía se muestra como un espacio."
  []
  (doseq [y (range alto)]
    (println (apply str (map #(char (obtener % y)) (range ancho))))))
