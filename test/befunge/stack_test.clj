;; Pruebas de manipulación de la pila

(ns befunge.stack-test
  (:require [clojure.test :refer :all]
            [befunge.stack :as stack]))

(deftest test-pila-basica
  (testing "Operaciones básicas de la pila"
    (stack/limpiar-pila)
    (stack/apilar 5)
    (is (= [5] (stack/ver-pila)))
    (stack/apilar 10)
    (is (= [5 10] (stack/ver-pila)))
    (is (= 10 (stack/desapilar)))
    (is (= [5] (stack/ver-pila))))

  (testing "Duplicar valor superior"
    (stack/apilar 20)
    (stack/duplicar)
    (is (= [5 20 20] (stack/ver-pila)))))

(run-tests)