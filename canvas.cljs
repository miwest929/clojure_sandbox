(ns game)

(def rows 20)
(def cols 10)
(def block-width 10)
(def block-height 10)
(def grid (apply vector (take (* rows cols) (repeatedly #(rand-int 2)))))
(defn x-coord [idx] (int (/ idx cols)))
(defn y-coord [idx] (mod idx cols))
(defn get-grid-blocks [grid]
  (map-indexed ; %1 => index, %2 => element
    #(cond (= 0 %2) nil
           :else {:x (* (x-coord %1) block-height) :y (* (y-coord %2) block-width) :color "white"})
    grid))

(def key-states (atom {}))

(defn context [width height]
  (let [target (.getElementById js/document "qwirkle")]
    [(.getContext target "2d") 
     (set! (. target -width) width)
     (set! (. target -height) height)]))

(render-scene-blocks [ctx]
  (let [blocks (get-grid-blocks grid)]
    (doseq [block blocks] 
      (set! (. ctx -fillStyle) "#000")
      (.fillRect ctx (block :x) (block :y) block-width block-height)
      (set! (. ctx -fillStyle) "#FFF")
      (.fillRect ctx (- (block :x) 3) (- (block :y) 3) (- block-width 3) (- block-height 3))
    )))

(defn ^:export init []
  (hook-input-events)
  (let [ctx (context 800 600)]
    (render-scene-blocks ctx)))

(defn set-key-state [code, value]
    (swap! key-states assoc code value))
