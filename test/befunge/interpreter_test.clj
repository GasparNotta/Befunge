;; Pruebas de lectura y representación del código

(ns befunge.interpreter-test
  (:require [clojure.test :refer :all]
            [befunge.interpreter :as interpreter]
            [befunge.stack :as stack]))

(deftest test-ejecutar-comando
  (testing "Comandos de la pila"
    (stack/limpiar-pila)
    (interpreter/ejecutar-comando \5)
    (is (= [5] (stack/ver-pila)))
    (interpreter/ejecutar-comando \8)
    (is (= [5 8] (stack/ver-pila)))
    (interpreter/ejecutar-comando \+)
    (is (= [13] (stack/ver-pila))))

  (testing "Comando de salida"
    (with-out-str
      (interpreter/ejecutar-comando \.)
      (is (= [] (stack/ver-pila))))))

(deftest test-direccion-pc
  (testing "Cambio de dirección del PC"
    (is (= :derecha (interpreter/cambiar-direccion \>)))
    (is (= :izquierda (interpreter/cambiar-direccion \<)))
    (is (= :abajo (interpreter/cambiar-direccion \v)))
    (is (= :arriba (interpreter/cambiar-direccion \^)))))

(run-tests)
