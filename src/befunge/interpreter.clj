(ns befunge.interpreter)

(def direcciones {:derecha [0 1], :izquierda [0 -1], :arriba [-1 0], :abajo [1 0]})

(defn crear-entorno [programa]
  {:programa programa, :posicion [0 0], :direccion :derecha, :modo-cadena false, :pila '()})

(defn mover [{:keys [posicion direccion programa] :as entorno}]
  (let [[x y] posicion, [dx dy] (direcciones direccion)
        bordear (fn [valor max] (mod (+ valor max) max))]
    (-> entorno
        (update :posicion
                #(vector (bordear (+ (first %) dx) (count programa))
                         (bordear (+ (second %) dy) (count (first programa))))))))

(defn apilar [entorno valor]
  (update entorno :pila conj valor))

(defn desapilar [entorno]
  [(or (first (:pila entorno)) 0) (update entorno :pila rest)])

(defn interpretar-comando [entorno comando]
  (if (:modo-cadena entorno)
  (if (= comando \") (update entorno :modo-cadena not) (apilar entorno (int comando)))
    (case comando
      \> (assoc entorno :direccion :derecha)
      \< (assoc entorno :direccion :izquierda)
      \^ (assoc entorno :direccion :arriba)
      \v (assoc entorno :direccion :abajo)
      \? (assoc entorno :direccion (rand-nth (keys direcciones)))
      \_ (let [[a entorno] (desapilar entorno)] (assoc entorno :direccion (if (zero? a) :derecha :izquierda)))
      \| (let [[a entorno] (desapilar entorno)] (assoc entorno :direccion (if (zero? a) :abajo :arriba)))
      \! (let [[a entorno] (desapilar entorno)] (apilar entorno (if (zero? a) 1 0)))
      \` (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (if (> b a) 1 0)))
      \" (update entorno :modo-cadena not)
      \. (let [[num entorno] (desapilar entorno)] (when num (print (str num " "))) entorno)
      \, (let [[chr entorno] (desapilar entorno)] (when chr (print (char chr))) entorno)
      \& (apilar entorno (Integer/parseInt (read-line)))
      \~ (apilar entorno (int (first (read-line))))
      \: (let [tope (or (first (:pila entorno)) 0)] (apilar entorno tope))
      \\ (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)](-> entorno (apilar a) (apilar b)))
      \$ (second (desapilar entorno))
      \+ (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (+ b a)))
      \- (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (- b a)))
      \* (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (* b a)))
      \/ (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (if (= a 0) 0 (quot b a))))
      \% (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (apilar entorno (if (= a 0) 0 (mod b a))))
      \# (mover entorno)
      \p (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno), [c entorno] (desapilar entorno)] (update entorno :programa update-in [a b] (constantly (char c))))
      \g (let [[a entorno] (desapilar entorno), [b entorno] (desapilar entorno)] (if (and (< a (count (:programa entorno))) (< b (count (first (:programa entorno)))))
                                                                                  (apilar entorno (int (get-in (:programa entorno) [a b])))
                                                                                  (apilar entorno 0)))
      (if (Character/isDigit comando) (apilar entorno (Character/digit comando 10)) entorno))))

(defn ejecutar-programa [programa]
  (loop [entorno (crear-entorno programa)]
    (if (= (get-in (:programa entorno) (:posicion entorno)) \@) nil
      (recur (-> entorno (interpretar-comando (get-in (:programa entorno) (:posicion entorno))) mover)))))