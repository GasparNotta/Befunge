;; Pruebas de manipulación de la pila

(ns befunge.stack-test
  (:require [clojure.test :refer :all]
            [befunge.stack :as stack]))

(deftest test-apilar
  (stack/apilar 5)
  (is (= (stack/mostrar-pila) [5])))

(deftest test-desapilar
  (stack/apilar 5)
  (is (= (stack/desapilar) 5))
  (is (= (stack/mostrar-pila) [])))

(deftest test-sumar
  (stack/apilar 5)
  (stack/apilar 3)
  (stack/sumar)
  (is (= (stack/mostrar-pila) [8])))

;; Agrega más pruebas según sea necesario

(run-tests)