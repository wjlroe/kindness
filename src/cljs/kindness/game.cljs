(ns kindness.game
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as async
             :refer [>! <! put! chan alts! timeout]]
            [clojure.set :refer [difference]]
            [kindness.gamepad :as gamepad]
            [kindness.utils :as utils])
  (:import [goog.events EventType]))

(enable-console-print!)

(defonce game-width 900)
(defonce game-height 600)
(def keyboard-controls
  {37 :left
   39 :right
   38 :up
   40 :down
   87 :up ;; W
   65 :left ;; A
   83 :down ;; S
   68 :right ;; D
   69 :activate ;; E
   32 :play-pause ;; space bar
   191 :toggle-music ;; /
   })
(def gamepad-buttons
  {0 :activate
   1 :play-pause})
(def keyboard-move-speed 20)
(def gamepad-move-speed 7)

(def styles
  {:body {:margin 0}
   :canvas {:image-rendering "pixelated"}})

(def image-assets
  {:owl "owl.png"
   :cage "cage.png"
   :player "person.png"})

(defn text-entity
  [entity]
  (:text (second entity)))

(defn default-components
  [entity idx]
  (let [entity-name (first entity)]
    (cond
      (= :background entity-name)
      [:renderable (merge {:shape :background
                           :z idx}
                          (second entity))]

      (text-entity entity)
      [:renderable {:text (text-entity entity)
                    :z idx
                    :shape :text}]

      (get image-assets (first entity))
      [:renderable {:image (first entity)
                    :shape :image
                    :z idx}]

      :else
      [:renderable (merge
                    {:shape :rectangle
                     :color "#AB4642"
                     :z idx}
                    (second entity))])))

(defn build-components-array [name idx attrs]
  (filter
   identity
   (for [[k v] attrs]
     (condp = k
       :position [:positionable {:position v}]
       :velocity [:movable {:velocity v}]
       :bounds [:boundable {:bounds v}]
       :controls [:controllable {:controls v}]
       :weapons [:weaponised v]
       :trigger [:triggerable v]
       :advance [:advancable v]
       nil))))

(defn make-entity
  ([entity] (make-entity 100 entity))
  ([idx entity]
   (let [name (first entity)
         attrs (second entity)
         defaults (default-components entity idx)
         components (build-components-array name idx attrs)]
     {:id (utils/guid)
      :name name
      :components (into {} (cons defaults components))})))

(defn build-entity-array [entities]
  (keep-indexed make-entity
                entities))

(defn new-owl
  [position]
   [:owl {:position position :bounds [48 48]
          :weapons {:laser {:every (* 1000 (inc (rand-int 15)))}}
          :trigger {:action :release
                    :proximity 80}}])

(def new-game-entities
  [[:background {:color "#33CC00"}]
   [:player {:position [50 300] :bounds [48 48]
             :controls [:up :down :left :right]}]

   (new-owl [650 100])
   [:cage {:position [650 100] :bounds [48 48]}]

   (new-owl [600 250])
   [:cage {:position [600 250] :bounds [48 48]}]

   (new-owl [700 200])
   [:cage {:position [700 200] :bounds [48 48]}]

   (new-owl [500 400])
   [:cage {:position [500 400] :bounds [48 48]}]

   [:instructions {:position [450 595] :bounds [900 32]
                   :text {:align "center"
                          :baseline "bottom"
                          :font "32pt Munro"
                          :color "#fefefe"
                          :text "←↑↓→ for movement. 'E' to release nearby owl"}}]])

(def start-screen
  [[:spacebar {:advance {:next :start}}]
   [:background {:color "#CC66FF"}]
   [:title {:position [450 200]
            :text {:align "center"
                   :baseline "middle"
                   :font "78pt Munro"
                   :color "#383838"
                   :text "Kindness to owls!"}}]
   [:title {:position [450 400]
            :text {:align "center"
                   :baseline "middle"
                   :font "48pt Munro"
                   :color "#383838"
                   :text "Press Space to start"}}]
   [:player {:position [450 500]}]])

(def lose-screen
  [[:spacebar {:advance {:next :start}}]
   [:background {:color "#FF6600"}]
   [:title {:position [450 200]
            :text {:align "center"
                   :baseline "middle"
                   :font "78pt Munro"
                   :color "#383838"
                   :text "Oh no! :("}}]
   [:title {:position [450 300]
            :text {:align "center"
                   :baseline "middle"
                   :font "42pt Munro"
                   :color "#383838"
                   :text "You got caught in an owl's laser beam."}}]
   [:title {:position [450 400]
            :text {:align "center"
                   :baseline "middle"
                   :font "32pt Munro"
                   :color "#383838"
                   :text "Don't hold it against them - they can't help it"}}]])

(def win-screen
  [[:spacebar {:advance {:next :start}}]
   [:background {:color "#FFCC66"}]
   [:title {:position [450 200]
            :text {:align "center"
                   :baseline "middle"
                   :font "78pt Munro"
                   :color "#383838"
                   :text "Well done!"}}]
   [:title {:position [450 300]
            :text {:align "center"
                   :baseline "middle"
                   :font "48pt Munro"
                   :color "#383838"
                   :text "You set all the owls free!"}}]
   [:title {:position [450 400]
            :text {:align "center"
                   :baseline "middle"
                   :font "32pt Munro"
                   :color "#383838"
                   :text "You are truly a friend to all owl-kind."}}]])

(def base-game-state
  {:entities (build-entity-array start-screen)})

(defonce game-state (atom base-game-state))
(defonce rafchan (utils/raf-to-chan))
(defonce control-chan (chan 1))
(defonce game-booted (atom nil))

(defn start-game
  []
  (swap! game-state assoc :entities (build-entity-array new-game-entities)))

(defn win-game
  []
  (swap! game-state assoc :entities (build-entity-array win-screen)))

(defn lose-game
  []
  (swap! game-state assoc :entities (build-entity-array lose-screen)))

(defn remove-entity
  [entity]
  (let [new-entities (remove (fn [e] (= (:id e) (:id entity)))
                             (:entities @game-state))]
    (swap! game-state assoc :entities new-entities)))

(defn replace-entity
  [entity]
  (let [others (remove (fn [e] (= (:id e) (:id entity)))
                       (:entities @game-state))]
    (swap! game-state assoc :entities (conj others entity))))

(defn insert-entity
  [entity]
  (swap! game-state assoc :entities (conj (:entities @game-state) entity)))

(defn upgrade-game-state []
  (let [missing-ks (difference (set (keys base-game-state))
                               (set (keys @game-state)))]
    (when (seq missing-ks)
      (let [missing-vals ((apply juxt missing-ks) base-game-state)]
        (println (swap! game-state merge (zipmap missing-ks missing-vals)))))))

(defn generate-laser
  [now entity]
  (let [[x y w h] (utils/position-and-bounds entity)
        laser (make-entity [:laser {:position [(- x 48) (+ y 10)]
                                    :bounds [48 2]
                                    :color "#CCFF66"
                                    :velocity [-0.4 0]}])
        new-entity (assoc-in entity [:components :weaponised :laser :last] now)]
    (replace-entity new-entity)
    (insert-entity laser)))

(defn game-surface
  []
  (utils/create-element-if-not-exist "game" "canvas"))

(defn find-entities-by-component [component]
  (filter (fn [entity] (utils/find-component component entity))
          (:entities @game-state)))

(defn insert-font-css
  []
  (let [contents "@font-face {
   font-family: 'Munro';
   src: url('fonts/Munro.ttf');
   }"
        container (utils/create-element-if-not-exist "fontcss" "style" "head")]
    (set! (.-type container) "text/css")
    (set! (.-innerHTML container) contents))
  )

(defn setup-game-audio
  "Insert nodes into the dom such that the game audio tracks will be
  ready to play."
  []
  ;;
  (let [audio (utils/create-element-if-not-exist "soundtrack" "audio" "body")
        source-ogg (utils/create-element-if-not-exist "soundtrack-source-ogg" "source" "audio")
        source-mp3 (utils/create-element-if-not-exist "soundtrack-source-mp3" "source" "audio")]
    (set! (.-loop audio) 1)
    (set! (.-autoplay audio) 1)
    (set! (.-src source-ogg) "music/soundtrack.ogg")
    (set! (.-type source-ogg) "audio/ogg; codecs=\"vorbis\"")
    (set! (.-src source-mp3) "music/soundtrack.mp3")
    (set! (.-type source-mp3) "audio/mpeg; codecs=\"mp3\"")
))

(defmulti draw (fn [e] (:shape (utils/find-component :renderable e))))
(defmethod draw :circle [e]
  (let [renderable (utils/find-component :renderable e)
        color (:color renderable)
        positionable (utils/find-component :positionable e)
        [x y] (:position positionable)
        boundable (utils/find-component :boundable e)
        [w _] (:bounds boundable)
        canvas (:canvas @game-state)
        width (.-width canvas)
        height (.-height canvas)
        radius (int (* height (/ (/ w 2) 100)))
        surface (:surface @game-state)]
    (set! (.-fillStyle surface) color)
    (.beginPath surface)
    (.arc surface x y radius 0 (* 2 Math/PI) true)
    (.closePath surface)
    (.fill surface)))
(defmethod draw :rectangle [e]
  (let [renderable (utils/find-component :renderable e)
        color (:color renderable)
        positionable (utils/find-component :positionable e)
        [x y] (:position positionable)
        boundable (utils/find-component :boundable e)
        [width height] (:bounds boundable)
        surface (:surface @game-state)]
    ;;(println "name:" (:name e) "x,y:" [x y] "width, height:" [width height] "color:" color)
    (set! (.-fillStyle surface) color)
    (.fillRect surface x y width height)))
(defmethod draw :background [e]
  (let [renderable (utils/find-component :renderable e)
        color (:color renderable)
        canvas (:canvas @game-state)
        width (.-width canvas)
        height (.-height canvas)
        surface (:surface @game-state)]
    (set! (.-fillStyle surface) color)
    (.fillRect surface 0 0 width height)
    (.strokeRect surface 0 0 width height)))
(defmethod draw :image [e]
  (let [renderable (utils/find-component :renderable e)
        positionable (utils/find-component :positionable e)
        image (:image renderable)
        img (utils/find-image image)
        [x y] (:position positionable)
        surface (:surface @game-state)]
    (.drawImage surface img x y)))
(defmethod draw :text [e]
  (let [renderable (utils/find-component :renderable e)
        [x y] (:position (utils/find-component :positionable e))
        surface (:surface @game-state)
        canvas (:canvas @game-state)
        text (:text renderable)]
    (set! (.-textAlign surface) (:align text))
    (set! (.-textBaseline surface) (:baseline text))
    (set! (.-font surface) (:font text))
    (set! (.-fillStyle surface) (:color text))
    (.fillText surface (:text text) x y)))

(defn z-order
  [e]
  (get-in e [:components :renderable :z]))

(defn renderable-entities
  []
  (sort-by z-order
           (filter (fn [entity]
                     (utils/find-component :renderable entity))
                   (:entities @game-state))))

(defn render-entities
  []
  (doseq [entity (renderable-entities)]
    (draw entity)))

(defn tap-print
  [text val]
  ;; (when val
  ;;   (println text))
  val)

(defn object-center
  [entity]
  (let [[x y w h] (utils/position-and-bounds entity)]
    [(int (+ x (/ w 2)))
     (int (+ y ( / h 2)))]))

(defn entity-proximity?
  [e1 e2 minimum]
  (let [[[x1 y1] [x2 y2]] (map object-center [e1 e2])
        proximity (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                   (Math/pow (- y1 y2) 2)))]
    ;; (println "object centers:" (map object-center [e1 e2]) "proximity:" proximity "min:" minimum)
    (<= proximity minimum)))

(defn collide?
  [& both]
  {:pre [(= (count both) 2)]}
  (let [boundables (map (partial utils/find-component :boundable) both)
        [[x1 y1 w1 h1] [x2 y2 w2 h2]] (map utils/position-and-bounds both)]
    ;; nobody can collide with
    ;; an entity that doesn't have bounds
    (and (= (count (remove nil? boundables)) (count both))
         (or (tap-print
              "top-right"
              (and (<= x1 x2 (+ x1 w1))
                   (<= y2 y1 (+ y2 h2))))
             (tap-print
              "bottom-right"
              (and (<= x1 x2 (+ x1 w1))
                   (<= y1 y2 (+ y1 h1))))
             (tap-print
              "bottom-left"
              (and (<= x2 x1 (+ x2 w2))
                   (<= y1 y2 (+ y1 h1))))
             (tap-print
              "top-left"
              (and (<= x2 x1 (+ x2 w2))
                   (<= y2 y1 (+ y2 h2))))))))

(defn collision?
  [entity others]
  (filter (fn [x] (collide? x entity))
          others))

(defn collision-detect
  [entity]
  (collision? entity
              (remove (fn [e] (= (:id e) (:id entity)))
                      (find-entities-by-component :boundable))))

(defn move-entity!
  [name dx dy]
  (let [entity (first (filter (fn [x] (= (:name x) name)) (:entities @game-state)))
        new-entity (update-in entity [:components :positionable :position]
                               (fn [[x y]]
                                 [(+ x dx) (+ y dy)]))]
    (when-not (seq (collision-detect new-entity))
      (replace-entity new-entity))))

(defn wrap-round
  [[x y]]
  [(if (> x 100) 0 x)
   (if (> y 100) 0 y)])

(defn new-position
  [[x y] [dx dy] delta]
  [(+ x (* delta dx))
   (+ y (* delta dy))])

(defn move-entities
  [delta]
  (doseq [entity (find-entities-by-component :movable)]
    (let [movable (utils/find-component :movable entity)
          velocity (:velocity movable)
          positionable (utils/find-component :positionable entity)
          position (:position positionable)
          npos (new-position position velocity delta)
          new-entity (update-in entity [:components :positionable :position] (constantly npos))
          collided-entites (collision-detect new-entity)
          collided-players (remove (fn [e] (= (:id e) (:id new-entity)))
                                   (filter (fn [e] (= :player (:name e))) collided-entites))]
      ;;(println "old position:" position "new:" npos)
      (if (seq collided-players)
        (do
          (println "entity" new-entity "collided with players:" collided-players)
          (lose-game))
        (replace-entity new-entity)))))

(defn entity-positions
  []
  (doseq [entity (find-entities-by-component :positionable)]
    (println "Entity with name" (:name entity)
             "has position:"
             (:position (utils/find-component :positionable entity)))))

(defn get-entities
  []
  (println (:entities @game-state)))

(defn print-entities
  []
  (doseq [entity (sort-by z-order
                          (:entities @game-state))]
    (println entity)))

(defn trigger-entity!
  [entity]
  (replace-entity (-> entity
                      (update-in [:components] dissoc :boundable :weaponised)
                      (assoc-in [:components :movable :velocity] [0 -0.5]))))

(defn try-activate!
  []
  (doseq [player (find-entities-by-component :controllable)]
    (doseq [entity (find-entities-by-component :triggerable)]
      (let [proximity (:proximity (utils/find-component :triggerable entity))]
        (when (entity-proximity? player entity proximity)
          (trigger-entity! entity))))))

(defn play-pause-game
  []
  (let [advancable (first (find-entities-by-component :advancable))
        next-state (:next (utils/find-component :advancable advancable))]
    (condp = next-state
      :start (start-game)
      nil)))

(defn action-dispatch
  [move-speed action]
  (condp = action
    :left (move-entity! :player (- move-speed) 0)
    :right (move-entity! :player move-speed 0)
    :up (move-entity! :player 0 (- move-speed))
    :down (move-entity! :player 0 move-speed)
    :activate (try-activate!)
    :play-pause (play-pause-game)
    :toggle-music (utils/pause-play-music)
    nil))

(defn keyboard-handler
  [event]
  ;; (println (.-keyCode event))
  (when-let [action (get keyboard-controls (.-keyCode event))]
    (action-dispatch keyboard-move-speed action)))

(defn gamepad-press
  [idx button]
  {:button idx :pressed (.-pressed button)})

(defn press-to-action
  [press]
  (when press
   (get gamepad-buttons (:button press))))

(defn axis-to-action
  [axes]
  (let [left-x (aget axes 0)
        left-y (aget axes 1)]
    (filter
     identity
     (list
      (when (< left-x -0.5) :left)
      (when (> left-x 0.5) :right)
      (when (< left-y -0.5) :up)
      (when (> left-y 0.5) :down)))))

(defn tap-val
  [prefix val]
  (println prefix val)
  val)

(defn gamepad-move
  [gamepads]
  (when-let [first-gamepad (aget gamepads 0)]
    (let [axes (.-axes first-gamepad)
          buttons (.-buttons first-gamepad)
          actions (concat (->> (keep-indexed gamepad-press buttons)
                               (filter :pressed)
                               (map press-to-action)
                               (filter identity))
                          (axis-to-action axes))]
      (doseq [action actions]
        (action-dispatch gamepad-move-speed action)))))

(defn outside-bounds?
  [entity width height]
  (let [[x y w h] (utils/position-and-bounds entity)]
    (or (<= (+ x w) 0)
        (>= y height)
        (>= x width)
        (<= (+ y h) 0))))

(defn cleanup-entities
  []
  (doseq [entity (find-entities-by-component :movable)]
    (let [canvas (:canvas @game-state)
          width (.-width canvas)
          height (.-height canvas)]
      (when (outside-bounds? entity width height)
        (remove-entity entity)))))

(defn lasers-zomg!
  [delta]
  (let [now (js/Date.)]
   (doseq [entity (find-entities-by-component :weaponised)]
     (let [weaponised (utils/find-component :weaponised entity)
           laser (:laser weaponised)
           interval (:every laser)
           last-time (:last laser)]
       (when (or (nil? last-time)
                 (>= (.getTime now) (+ (.getTime last-time) interval)))
         (generate-laser now entity))))))

(defn check-win-condition
  []
  (when (seq (find-entities-by-component :controllable))
    (when-not (seq (find-entities-by-component :weaponised))
      (win-game))))

(defn render
  [delta]
  (lasers-zomg! delta)
  (move-entities delta)
  (cleanup-entities)
  (check-win-condition)
  (render-entities))

(defn setup-canvas
  [canvas]
  (doto canvas
    (-> .-width (set! game-width))
    (-> .-height (set! game-height))
    ;; (-> .-tabindex (set! "1"))
    ))

(defn setup-styles
  [canvas]
  ;; TODO: we don't want to do this on Ludum Dare's webpages!!!
  (let [body (utils/get-first-elem "body")]
    (utils/set-element-style body (:body styles)))
  (utils/set-element-style canvas (:canvas styles)))

;; events->chan on keyboard -> println the event (check it's not duplicated)

(defn boot-game
  []
  (let [canvas (game-surface)
        ;; _ (println "at boot, canvas is:" canvas)
        surface (.getContext canvas "2d")
        ;; _ (println "at boot, 2d surface is:" surface)
        ;; TODO: listening on js/window might break LD pages!!!
        [keyboard-event-key kbd-chan] (utils/events->chan js/window
                                                          EventType.KEYDOWN
                                                          (chan 1))]
    (swap! game-state assoc :canvas canvas :surface surface)
    (setup-canvas canvas)
    (setup-styles canvas)
    (insert-font-css)
    (utils/insert-image-assets! image-assets)
    (setup-game-audio)
    (go-loop []
      (let [[v c] (alts! [kbd-chan rafchan control-chan])]
        (condp = c
          kbd-chan (do
                     (keyboard-handler v)
                     (recur))
          control-chan (if (= v :close)
                         (do
                           (utils/stop-events keyboard-event-key)
                           (println "Control closed"))
                         (recur))
          rafchan (do (gamepad-move (gamepad/raw-gamepads))
                      (render v)
                      (recur)))))))

(when-not @game-booted
  (boot-game)
  (reset! game-booted :yup))
