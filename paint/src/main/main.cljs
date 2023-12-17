(ns main
  (:require [render]))

; constants
(def block-size 20)
(def color-bg "black")
(def color-inactive "#3a3a3a")
(def colors ["#ed1411" ; red
             "#f7a81e" ; orange
             "#fff705" ; yellow
             "#32bf58" ; green
             "#33d3ff" ; light blue
             "#0059ff" ; blue
             "#8b17ff" ; violet
             color-bg])
(def color-empty-idx 7)

; dom elements
(def el-info (render/dom-by-id "info"))
(def el-canvas (render/dom-by-id "canvas"))
(def canvas-ctx (.getContext el-canvas "2d"))

(defn now-ms [] (.now js/Date))

; controls & rendering
(defn- key-number? [code] (<= 48 code 57))

(defn- key->number [code] (when (key-number? code) (- code 48)))

(defn code->key [code]
  (let [keys {32 :space
              37 :left
              38 :up
              39 :right
              40 :down}]
    (get keys code)))

(defn setup-keyboard [listener]
  (.addEventListener js/document
                     "keydown"
                     (fn [e]
                       (let [code (.-keyCode e)] (listener (code->key code) code)))))

(defn empty-canvas! [width height]
  (render/draw-rectangle! canvas-ctx color-bg 0 0 width height))

(defn setup-canvas []
  (let [width (- (quot (.-innerWidth js/window) block-size) 10)
        height (- (quot (.-innerHeight js/window) block-size) 4)
        width-px (* block-size width)
        height-px (* block-size height)]
    (render/dom-width-height! el-canvas width-px height-px)
    {:blocks [width height]
     :px [width-px height-px]}))

(defn setup-info-colors! [active-idx]
  (doseq [[idx color] (map-indexed vector colors)]
    (render/dom-create-el :div el-info {:classes ["color"
                                                  (when (= active-idx idx) "active")]
                                        :bg color
                                        :text (inc idx)})))

(defn- i->xy [i width] [(mod i width) (quot i width)])
(defn- xy->i [x y width] (+ (* width y) x))

(defn render-cursor! [{x :x, y :y, color-idx :color-idx, active :active} pic line-width]
  (let [pic-under (not= (pic (xy->i x y line-width)) color-empty-idx)
        color (if
           (= (colors color-idx) color-bg)
            color-inactive
            (if pic-under color-bg (colors color-idx)))
        px (* x block-size)
        py (* y block-size)]
    (render/stroke-rectangle! canvas-ctx color px py block-size block-size active)))

(defn render-pic! [pic line-width]
  (doseq [[idx color-idx] (map-indexed vector pic)]
    (let [[pos-x pos-y] (i->xy idx line-width)
          x (* block-size pos-x)
          y (* block-size pos-y)
          color (if (= color-idx color-empty-idx) color-bg (colors color-idx))]
      (render/draw-rectangle! canvas-ctx color x y block-size block-size))))

(defn update-active-color! [color-idx]
  (doseq [[idx, color-el] (keep-indexed vector (render/dom-get-children el-info))]
    (if (= idx color-idx)
      (render/dom-add-class color-el "active")
      (render/dom-remove-class color-el "active"))))

(defn within-canvas? [{x :x y :y} [canvas-width canvas-height]]
  (and (<= 0 x (dec canvas-width)) (<= 0 y (dec canvas-height))))

; state updates
(defn move-cursor [cursor key]
  (cond
    (= key :left)  (update cursor :x dec)
    (= key :right) (update cursor :x inc)
    (= key :down)  (update cursor :y inc)
    (= key :up)    (update cursor :y dec)
    (= key :space) (update cursor :active not)
    :else cursor))

(defn update-cursor [cursor canvas-size key]
  (let [next-state (move-cursor cursor key)]
    (if (within-canvas? next-state canvas-size) next-state cursor)))

(defn update-pic [cursor pic width]
  (let [{active :active, x :x, y :y, color-idx :color-idx} cursor]
    (if active
      (assoc pic (xy->i x y width) color-idx)
      pic)))

(defn init []
  (let [{canvas-px :px, canvas-blocks :blocks} (setup-canvas)
        canvas-width (canvas-blocks 0)
        canvas-height (canvas-blocks 1)
        state (atom {:render {:last-frame (now-ms)}
                     :cursor {:x (quot canvas-width 2)
                              :y (quot canvas-height 2)
                              :active false
                              :color-idx 0}
                     :pic (into [] (replicate (apply * canvas-blocks) color-empty-idx))})

        sget-in (fn [path] (get-in @state path))
        s! (fn [path val] (swap! state assoc-in path val))

        draw (fn []
               (apply empty-canvas! canvas-px)
               (render-pic! (:pic @state) canvas-width)
               (render-cursor! (:cursor @state) (:pic @state) canvas-width))

        keypress
        (fn [key code]
          (let [number (key->number code)]
            (when key
              (s! [:cursor] (update-cursor (:cursor @state) canvas-blocks key))
              (s! [:pic] (update-pic (:cursor @state) (:pic @state) canvas-width)))
            (when (contains? colors (dec number))
              (s! [:cursor :color-idx] (dec number))
              (s! [:pic] (update-pic (:cursor @state) (:pic @state) canvas-width))
              (update-active-color! (dec number)))))

        frame
        (fn [dt-ms]
          (let [now (now-ms)]
            ;; frame rate & counters
            (render/render-fps! "fps" dt-ms)
            (s! [:render :last-frame] now)
            (draw)))]
    (setup-keyboard keypress)
    (setup-info-colors! (sget-in [:cursor :color-idx]))
    (render/raf! frame)))
