(ns main
  (:require [render]
            [pvector]))

; constants
(def block-size 20)
(def color-bg "black")
(def color-inactive "#3a3a3a")
(def colors [color-bg
             "#ed1411" ; red
             "#f7a81e" ; orange
             "#fff705" ; yellow
             "#32bf58" ; green
             "#33d3ff" ; light blue
             "#0059ff" ; blue
             "#8b17ff" ; violet
             "#523400" ; brown
             "#8a8a8a"]) ; grey

(def color-empty-idx 0)

(defn get-random-color-idx []
  (+ 1 (rand-int (- (count colors) 3))))

(get-random-color-idx)

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
              40 :down
              ;; R
              82 :reset}]
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

;; TODO index translation
(defn setup-info-colors! [active-idx]
  (doseq [[idx color] (map-indexed vector (drop 1 colors))]
    (render/dom-create-el :div el-info {:classes ["color"
                                                  (when (= active-idx (inc idx)) "active")]
                                        :bg color
                                        :text (inc idx)}))
  (render/dom-create-el :div el-info {:classes ["color"
                                                (when (= active-idx 0) "active")]
                                      :bg (first colors)
                                      :text 0}))

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


(defn within-canvas? [{x :x y :y} [canvas-width canvas-height]]
  (and (<= 0 x (dec canvas-width)) (<= 0 y (dec canvas-height))))


(defn- frame-pendulum [n dir len]
  (let [next-n (+ n dir)
        last (dec len)]
    (cond
      (< next-n 0) [(min 1 last) 1]
      (>= next-n len) [(max 0 (dec n)) -1]
      :else [next-n dir])))


(defn boundaries [pos [w h]]
  [pos
   (pvector/add pos [w 0])
   (pvector/add pos [w h])
   (pvector/add pos [0 h])])

(defn- gnome-next-step [pos bps dir size-px canvas-px]
  (let [[x y] pos
        step (* bps block-size)
        diff {:down  [x (+ y step)]
              :right [(+ x step) y]
              :up    [x (- y step)]
              :left  [(- x step) y]}
        next-pos (diff dir)
        next-boundaries (boundaries next-pos size-px)]

    (if (every? (fn [[x y]] (within-canvas? {:x x :y y} canvas-px)) next-boundaries)
      [next-pos dir]
      (case dir
        :down [(diff :right) :right]
        :right [(diff :up) :up]
        :up [(diff :left) :left]
        :left [(diff :down) :down]))))

(defn update-gnome! [gnome dt-ms canvas-px]
  (let [{{fps :fps
          len :animation-len
          frame-size :frame-size} :sprite
         scale :scale
         frame :frame
         frame-dir :frame-dir
         frame-wait :frame-wait
         pos :pos
         dir :anim
         bps :bps} gnome
        ms-frame (/ 1000 fps)
        new-wait (+ frame-wait dt-ms)
        wait-diff (- ms-frame new-wait)
        should-render (>= 0 wait-diff)]
    (if should-render
      (let [[next-frame next-frame-dir] (frame-pendulum frame frame-dir len)
            [next-pos next-dir] (gnome-next-step pos bps dir (map #(* scale %) frame-size) canvas-px)]
        (-> gnome
            (assoc-in [:frame] next-frame)
            (assoc-in [:frame-dir] next-frame-dir)
            (assoc-in [:frame-wait] (abs wait-diff))
            (assoc-in [:pos] next-pos)
            (assoc-in [:anim] next-dir)))
      (assoc-in gnome [:frame-wait] new-wait))))

(defn render-gnome! [{sprite :sprite, pos :pos, frame :frame, a :anim, scale :scale}]
  (render/draw-sprite! canvas-ctx sprite a frame pos scale))

;; TODO index translation
(defn update-active-color! [active-idx]
  (doseq [[idx, color-el] (keep-indexed vector (render/dom-get-children el-info))]
    (if (or (= (inc idx) active-idx) (and (= active-idx 0) (= idx (dec (count colors)))))
      (render/dom-add-class color-el "active")
      (render/dom-remove-class color-el "active"))))

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

(defn- empty-pic [canvas-blocks]
  (into [] (replicate (apply * canvas-blocks) color-empty-idx)))

(defn init []
  (let [{canvas-px :px, canvas-blocks :blocks} (setup-canvas)
        canvas-width (canvas-blocks 0)
        canvas-height (canvas-blocks 1)
        state (atom {:render {:last-frame (now-ms)}
                     :cursor {:x (quot canvas-width 2)
                              :y (quot canvas-height 2)
                              :active false
                              :color-idx (get-random-color-idx)}
                    ;;  :prev (empty-pic canvas-blocks)
                     :pic (empty-pic canvas-blocks)
                     :gnome {:sprite render/gnome-sprite
                             :pos [0 0]
                             :anim :down
                             :frame 0
                             :frame-dir 1
                             :scale 2
                             :frame-wait 0
                             :bps 1}})

        sget-in (fn [path] (get-in @state path))
        s! (fn [path val] (swap! state assoc-in path val))

        draw (fn [dt-ms]
               (apply empty-canvas! canvas-px)
               (render-pic! (:pic @state) canvas-width)
               (render-cursor! (:cursor @state) (:pic @state) canvas-width)
               ;; gnome
               (s! [:gnome] (update-gnome! (:gnome @state) dt-ms canvas-px))
               (render-gnome! (:gnome @state))
              ;;  (println (:frame-wait (:gnome @state)))
               )


        keypress
        (fn [key code]
          (let [number (key->number code)]
            (when (= key :reset)
              (s! [:cursor :active] false)
              (s! [:pic] (empty-pic canvas-blocks)))
            (when (not= key :reset)
              (s! [:cursor] (update-cursor (:cursor @state) canvas-blocks key))
              (s! [:pic] (update-pic (:cursor @state) (:pic @state) canvas-width)))
            (when (contains? colors number)
              (s! [:cursor :color-idx] number)
              (s! [:pic] (update-pic (:cursor @state) (:pic @state) canvas-width))
              (update-active-color! number))))

        frame (fn [dt-ms]
                ;; (println (:gnome @state))
                (render/render-fps! "fps" dt-ms)
                (s! [:render :last-frame] (now-ms))
                (draw dt-ms))]



    (setup-keyboard keypress)
    (setup-info-colors! (sget-in [:cursor :color-idx]))
    (render/disable-antialising canvas-ctx)
    (render/raf! frame)))
