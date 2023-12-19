(ns pvector)

(defn pv [x y] [x y])

;; TODO vectors with multiple dimensions
(defn add [v1 v2] (map + v1 v2))

(defn sub [v1 v2] (map - v1 v2))

(defn mul [[x y] sc] [(* x sc) (* y sc)])

(defn div [[x y] sc] [(/ x sc) (/ y sc)])

(defn dot [v1 v2] (reduce + (map * v1 v2)))

;; http://buildnewgames.com/gamephysics/#checkpoint-2
(defn cross [v1 v2] (reduce - (map * v1 v2)))

;; (defn vec-rotate [v angle rv]
;;   (let [[x y] v]
;;     []))
