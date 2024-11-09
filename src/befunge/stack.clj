;; Aca van a ir las funciones para manipular la pila
(ns befunge.stack
  "Módulo para manipulación de la pila en el intérprete de Befunge-93.")

;; Definimos la pila como un vector en el cual el último elemento es la cima de la pila.
(def pila (atom []))

;; Función para apilar (push) un valor en la pila.
(defn apilar
  "Agrega un valor a la cima de la pila."
  [valor]
  (swap! pila conj valor))

;; Función para desapilar (pop) un valor de la pila.
(defn desapilar
  "Elimina y devuelve el valor en la cima de la pila.
   Si la pila está vacía, devuelve 0."
  []
  (if (empty? @pila)
    0
    (let [valor (peek @pila)]
      (swap! pila pop)
      valor)))

;; Función para duplicar el valor en la cima de la pila.
(defn duplicar
  "Duplica el valor en la cima de la pila.
   Si la pila está vacía, apila un 0."
  []
  (apilar (if (empty? @pila)
            0
            (peek @pila))))

;; Función para intercambiar los dos valores en la cima de la pila.
(defn intercambiar
  "Intercambia los dos valores en la cima de la pila.
   Si la pila tiene menos de dos elementos, apila un 0."
  []
  (if (< (count @pila) 2)
    (apilar 0)
    (let [top1 (desapilar)
          top2 (desapilar)]
      (apilar top1)
      (apilar top2))))

;; Función para descartar el valor en la cima de la pila.
(defn descartar
  "Descarta el valor en la cima de la pila."
  []
  (when (not (empty? @pila))
    (swap! pila pop)))

;; Función para vaciar la pila, útil para pruebas.
(defn vaciar-pila
  "Vacía la pila completamente."
  []
  (reset! pila []))

;; Función para mostrar el estado actual de la pila, útil para depuración.
(defn mostrar-pila
  "Devuelve la pila como una lista (para depuración)."
  []
  @pila)