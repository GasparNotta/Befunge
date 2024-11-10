;; Configuración del proyecto y dependencias

(defproject Befunge "0.1.0-SNAPSHOT"
  :description "Intérprete Befunge-93 en Clojure"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main befunge.core
  :source-paths ["src"]
  :resource-paths ["resources"])
