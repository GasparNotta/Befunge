;; Aca van a ir las funciones para manipular la pila
(ns befunge.stack
  "Módulo para manipulación de la pila en el intérprete de Befunge-93."
  (:require [clojure.core :refer :all]))

;; Definimos la pila como un vector en el cual el último elemento es la cima de la pila.
(def pila (atom [])) ;; Usamos un atom para mantener la pila mutable

;; Función para apilar (push) un valor en la pila.
(defn apilar
  "Agrega un valor a la cima de la pila."
  [valor]
  (swap! pila conj valor))
  ;(println @pila)           ;; ----------------------------Depuración---------------------------

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

;; Funciones aritméticas y de lógica para Befunge-93

(defn sumar
  "Suma los dos valores en la cima de la pila y apila el resultado."
  []

  ;;(println @pila)    ;; ----------------------------Depuración---------------------------
  
  (let [a (desapilar)
        b (desapilar)]
    (apilar (+ a b))))

(defn restar
  "Resta el segundo valor en la cima de la pila del primero y apila el resultado."
  []
  (let [a (desapilar)
        b (desapilar)]
    (apilar (- b a))))

(defn multiplicar
  "Multiplica los dos valores en la cima de la pila y apila el resultado."
  []
  (let [a (desapilar)
        b (desapilar)]
    (apilar (* a b))))

(defn dividir
  "Divide el segundo valor en la cima de la pila por el primero y apila el resultado.
   Si se intenta dividir por 0, apila 0 como resultado."
  []
  (let [a (desapilar)
        b (desapilar)]
    (apilar (if (zero? a) 0 (quot b a)))))

(defn modulo
  "Calcula el módulo del segundo valor en la cima de la pila respecto al primero y apila el resultado.
   Si se intenta calcular el módulo por 0, apila 0 como resultado."
  []
  (let [a (desapilar)
        b (desapilar)]
    (apilar (if (zero? a) 0 (mod b a))))) 
    

(defn negacion-logica
  "Realiza una negación lógica: apila 1 si el valor en la cima de la pila es 0, de lo contrario, apila 0."
  []
  (let [a (desapilar)]
    (apilar (if (zero? a) 1 0))))

(defn mayor-que
  "Realiza una comparación lógica: apila 1 si el valor en la cima de la pila es menor que el que le precede.
  En otro caso apila 0"
  []
  (let [a (desapilar)
        b (desapilar)]
    (apilar (if (< a b) 1 0))))

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


;; ----------------------------Funcion para Depuración---------------------------


;; Función para mostrar el contenido actual de la pila, útil para depuración.
(defn obtener-pila []
  "Obtiene el estado actual de la pila."
  @pila)