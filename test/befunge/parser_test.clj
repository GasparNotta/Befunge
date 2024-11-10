;; Pruebas específicas de comandos y flujo

(ns befunge.parser-test
  (:require [clojure.test :refer :all]
            [befunge.parser :as parser]
            [befunge.torus :as torus]))

(deftest test-cargar-programa
  (testing "Cargar programa en el toroide"
    (let [programa ["v    "
                    ">987v"
                    "v456<"
                    ">321 ^"
                    "     @"]]
      (parser/cargar-programa programa)
      (is (= (torus/obtener 0 0) (int \v)))
      (is (= (torus/obtener 1 1) (int \>)))
      (is (= (torus/obtener 1 2) (int \v)))
      (is (= (torus/obtener 5 4) (int \@))))))

(run-tests)