(ns render)

(defn now-ms [] (.now js/Date))

(defn dom-by-id [id]
  (.getElementById js/document id))

(defn dom-text! [el text]
  (set! (.-innerText el) text))

(defn dom-get-children [el]
  (.-childNodes el))

(defn dom-add-class [el class]
  (.add (.-classList el) class))

(defn dom-remove-class [el class]
  (.remove (.-classList el) class))

(defn dom-create-el [type parent & rest]
  (let [tag (cond (= type :span) "span"
                  (= type :div) "div")
        {text :text
         classes :classes
         bg :bg} rest
        el (.createElement js/document tag)]
    (when (count classes)
      (doseq [class classes]
        (dom-add-class el class)))
    (when bg (set! (.-style el) (str "background: " bg)))
    (when text (dom-text! el text))
    (->> el
         (.appendChild parent))))

(defn- fps [n-frames dt] (->> (/ dt 1000) (/ n-frames)))

(defn render-fps! [element-id dt]
  (let [rate (.floor js/Math (fps 1 dt))]
    (dom-text! (dom-by-id element-id) rate)))

(defn dom-width-height! [el wpx hpx]
  (set! (.-width el) wpx)
  (set! (.-height el) hpx))

(defn draw-rectangle! [ctx style x y sx sy & rest]
  (let [opacity (or (first rest) 1)
        set-opacity (not= 1 opacity)]
    (when set-opacity
      (set! (.-globalAlpha ctx) opacity))
    (set! (.-fillStyle ctx) style)
    (.fillRect ctx x y sx sy)
    (when set-opacity
      (set! (.-globalAlpha ctx) 1))))

(defn stroke-rectangle! [ctx style x y w h active]
  (let [ax (+ (if active 4 0) x 2)
        ay (+ (if active 4 0) y 2)
        aw (- w 3 (if active 8 0))
        ah (- h 3 (if active 8 0))]
    (set! (.-strokeStyle ctx) style)
    (set! (.-lineWidth ctx) (if active 5 1))
    (.strokeRect ctx ax ay aw ah)
    (set! (.-lineWidth ctx) 1)))

(defn raf! [frame]
  (let [last-frame (atom (now-ms))
        wrap (fn wrap []
               (let [now (now-ms)]
                 (frame (- now @last-frame))
                 (reset! last-frame now)
                 (.requestAnimationFrame js/window wrap)))]
    (wrap)))
