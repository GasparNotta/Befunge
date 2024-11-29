(ns befunge.stack
  "Módulo para manipulación de la pila en el intérprete de Befunge-93."
  (:require [clojure.core :refer :all]))

;; Función para apilar (push) un valor en la pila.
(defn apilar
  "Agrega un valor a la cima de la pila."
  [pila valor]
  (conj pila valor))

;; Función para desapilar (pop) un valor de la pila.
(defn desapilar
  "Elimina y devuelve el valor en la cima de la pila.
   Si la pila está vacía, devuelve 0."
  [pila]
  (if (empty? pila)
    [0 pila]
    [(peek pila) (pop pila)]))

;; Función para obtener el valor superior de la pila sin eliminarlo (como peek)
(defn cima
  "Devuelve el valor en la cima de la pila sin eliminarlo.
   Si la pila está vacía, devuelve 0."
  [pila]
  (if (empty? pila) 0 (peek pila)))

;; Funciones aritméticas y de lógica para Befunge-93

(defn sumar
  "Suma los dos valores en la cima de la pila y apila el resultado."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (+ a b))))

(defn restar
  "Resta el segundo valor en la cima de la pila del primero y apila el resultado."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (- b a))))

(defn multiplicar
  "Multiplica los dos valores en la cima de la pila y apila el resultado."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (* a b))))

(defn dividir
  "Divide el segundo valor en la cima de la pila por el primero y apila el resultado.
   Si se intenta dividir por 0, apila 0 como resultado."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (if (zero? a) 0 (quot b a)))))

(defn modulo
  "Calcula el módulo del segundo valor en la cima de la pila respecto al primero y apila el resultado.
   Si se intenta calcular el módulo por 0, apila 0 como resultado."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (if (zero? a) 0 (mod b a)))))

(defn negacion-logica
  "Realiza una negación lógica: apila 1 si el valor en la cima de la pila es 0, de lo contrario, apila 0."
  [pila]
  (let [[a pila1] (desapilar pila)]
    (apilar pila1 (if (zero? a) 1 0))))

(defn mayor-que
  "Realiza una comparación lógica: apila 1 si el valor en la cima de la pila es menor que el que le precede.
  En otro caso apila 0"
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
    (apilar pila2 (if (< a b) 1 0))))

;; Función para duplicar el valor en la cima de la pila.
(defn duplicar
  "Duplica el valor en la cima de la pila.
   Si la pila está vacía, apila un 0."
  [pila]
  (apilar pila (cima pila)))

;; Función para intercambiar los dos valores en la cima de la pila.
(defn intercambiar
  "Intercambia los dos valores en la cima de la pila.
   Si la pila tiene menos de dos elementos, apila un 0."
  [pila]
  (let [[a pila1] (desapilar pila)
        [b pila2] (desapilar pila1)]
  (-> pila2
      (apilar a)
      (apilar b))))

;; Función para descartar el valor en la cima de la pila.
(defn descartar
  "Descarta el valor en la cima de la pila."
  [pila]
  (when-not (empty? pila)
    (pop pila)))