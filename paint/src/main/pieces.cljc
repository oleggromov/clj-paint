(ns pieces)

;; ..x  .x.  ...  xx.
;; xcx  .c.  xcx  .c.
;; ...  .xx  x..  .x.
(def club-right
  [[[1 -1] [-1 0] [0 0] [1 0]]
   [[1 1] [0 -1] [0 0] [0 1]]
   [[-1 0] [0 0] [1 0] [-1 1]]
   [[-1 -1] [0 -1] [0 0] [0 1]]])

;; x..  .xx  ...  .x.
;; xcx  .c.  xcx  .c.
;; ...  .x.  ..x  xx.
(def club-left
  [[[-1 -1] [-1 0] [0 0] [1 0]]
   [[0 -1] [1 -1] [0 0] [0 1]]
   [[-1 0] [0 0] [1 0] [1 1]]
   [[0 -1] [0 0] [-1 1] [0 1]]])

;; xx.  ..x
;; .cx  .cx
;; ...  .x.
(def steps-left
  [[[-1 -1] [0 -1] [0 0] [1 0]]
   [[1 -1] [0 0] [1 0] [0 1]]])

;; .xx  .x.
;; xc.  .cx
;; ...  ..x
(def steps-right
  [[[0 -1] [1 -1] [-1 0] [0 0]]
   [[0 -1] [0 0] [1 0] [1 1]]])

;; .x.  .x.  ...  .x.
;; xcx  .cx  xcx  xc.
;; ...  .x.  .x.  .x.
(def teewee
  [[[0 -1] [-1 0] [0 0] [1 0]]
   [[0 -1] [0 0] [1 0] [0 1]]
   [[-1 0] [0 0] [1 0] [0 1]]
   [[0 -1] [-1 0] [0 0] [0 1]]])

(def brick
  [[[0 0] [0 1] [1 0] [1 1]]])

;; ....  .x..
;; xcxx  .c..
;; ....  .x..
;; ....  .x..
(def pole
  [[[-1 0] [0 0] [1 0] [2 0]]
   [[0 -1] [0 0] [0 1] [0 2]]])

(defn next-rotation [figure cur]
  (if (< cur (dec (count figure))) (inc cur) 0))

(defn random-piece []
  (let
   [pieces [club-right
            club-left
            steps-right
            steps-left
            teewee
            brick
            pole]]
    (rand-nth pieces)))

(defn coords [piece cx cy] (map #(map + % [cx cy]) piece))

(defn centre-mass [piece cx cy]
  (let [c (coords piece cx cy)
        len (count piece)]
    [(/ (reduce + (map (comp (partial + 0.5) first) c)) len)
     (/ (reduce + (map (comp (partial + 0.5) second) c)) len)]))
