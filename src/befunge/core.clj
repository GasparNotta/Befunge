;; Aca va a ir el punto de entrada y la función `-main`

(ns befunge.core)

(def direcciones
  {:derecha [1 0], :izquierda [-1 0], :arriba [0 -1], :abajo [0 1]})

(defn cargar-archivo [archivo]
  (let [lineas (slurp archivo)]
    (vec (map vec (clojure.string/split-lines lineas)))))

(defn obtener-celda [toroide x y]
  (get-in toroide [y x]))

;Puse estas funciones en el archivo stack.clj
(defn apilar [pila val]
  (conj pila val))
(defn desapilar [pila]                                      ; Devuelve dato y resto de la pila
  (if (empty? pila)
    [0 pila]
    [(first pila) (rest pila)]))




(defn correr-instruccion [instruccion pila dir toroide x y]
  (case instruccion
    \+ (let [[a pila'] (desapilar pila)
             [b pila''] (desapilar pila')]
         [(apilar pila'' (+ a b)) dir toroide x y])
    \- (let [[a pila'] (desapilar pila)
             [b pila''] (desapilar pila')]
         [(apilar pila'' (- a b)) dir toroide x y])
    \* (let [[a pila'] (desapilar pila)
             [b pila''] (desapilar pila')]
         [(apilar pila'' (* a b)) dir toroide x y])
    \/ (let [[a pila'] (desapilar pila)
             [b pila''] (desapilar pila')]
         [(apilar pila'' (if (= b 0) 0 (quot a b))) dir toroide x y])
    \% (let [[a pila'] (desapilar pila)
             [b pila''] (desapilar pila')]
         [(apilar pila'' (mod a b)) dir toroide x y])       ; Manejar division entre 0
    ))

(defn -main [& args]
  (let [archivo (first args)
        programa (cargar-archivo archivo)]
    (println programa)))
