(ns kindness.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as async
             :refer [>! <! put! chan alts! timeout]]
            [clojure.set :refer [difference]]
            [figwheel.client :as fw]
            [kindness.utils :as utils])
  (:import [goog.events EventType]))

(enable-console-print!)

(defonce game-width 900)
(defonce game-height 600)
(def keyboard-controls
  {37 :left
   39 :right
   38 :up
   40 :down})
(def move-speed 20)

(def styles
  {:body {:margin 0}
   :canvas {:image-rendering "pixelated"}})

(def image-assets
  {:owl "owl.png"
   :cage "cage.png"})


(defn default-components
  [entity idx]
  (let [entity-name (first entity)]
    (if (= :background entity-name)
      [:renderable (merge {:shape :background
                           :z idx}
                          (second entity))]
      [:renderable (if (get image-assets (first entity))
                     {:image (first entity)
                      :shape :image
                      :z idx}
                     ;; make the failure case visually obvious
                     {:shape :rectangle
                      :color "#AB4642"
                      :z idx})])))

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
          :weapons {:laser {:every 10000}}}])

(def new-game-entities
  [[:background {:color "#33CC00"}]
   [:player {:position [50 250] :bounds [20 20]
             :controls [:up :down :left :right]}]
   (new-owl [600 250])
   [:cage {:position [600 250] :bounds [48 48]}]
   (new-owl [700 200])
   [:cage {:position [700 200] :bounds [48 48]}]])

(def base-game-state
  {:entities (build-entity-array new-game-entities)})

(defonce game-state (atom base-game-state))
(defonce rafchan (utils/raf-to-chan))
(defonce control-chan (chan 1))
(defonce game-booted (atom nil))

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
                                    :color "#3C6890"
                                    :velocity -0.4}])
        new-entity (assoc-in entity [:components :weaponised :laser :last] now)]
    (replace-entity new-entity)
    (insert-entity laser)))

(defn game-surface
  []
  (utils/create-element-if-not-exist "game" "canvas"))

(defn find-entities-by-component [component]
  (filter (fn [entity] (utils/find-component component entity))
          (:entities @game-state)))

(defn setup-game-audio
  "Insert nodes into the dom such that the game audio tracks will be
  ready to play."
  []
  ;;
  )


(defn position->corrds
  [[px py] width height shape-height shape-width]
  [(int (- (* width (/ px 100)) (/ shape-width 2)))
   (int (- (* height (/ py 100)) (/ shape-height 2)))])

(defn absolute-bounds
  [[w h] width height]
  [(int (* width (/ w 100)))
   (int (* height (/ h 100)))])

(defmulti draw (fn [e] (:shape (utils/find-component :renderable e))))
(defmethod draw :circle [e]
  (let [renderable (utils/find-component :renderable e)
        color (:color renderable)
        positionable (utils/find-component :positionable e)
        position (:position positionable)
        boundable (utils/find-component :boundable e)
        [w _] (:bounds boundable)
        canvas (:canvas @game-state)
        width (.-width canvas)
        height (.-height canvas)
        radius (int (* height (/ (/ w 2) 100)))
        surface (:surface @game-state)
        [x y] (position->corrds position width height (* 2 radius) (* 2 radius))]
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
        ;; canvas (:canvas @game-state)
        ;; width (.-width canvas)
        ;; height (.-height canvas)
        ;; [shape-width shape-height] (absolute-bounds bounds width height)
        surface (:surface @game-state)
        ;; [x y] (position->corrds position width height shape-height shape-width)
        ]
    ;;(println "name:" (:name e) "x,y:" [x y] "width, height:" [width height] "color:" color)
    (set! (.-fillStyle surface) color)
    (.fillRect surface x y width height)
    (set! (.-lineWidth surface) 0)
    (.strokeRect surface x y width height)))
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

(defn collide?
  [a b]
  (let [both [a b]
        [[x1 y1 w1 h1] [x2 y2 w2 h2]] (map utils/position-and-bounds both)]
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
              (<= y2 y1 (+ y2 h2)))))))

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
  [[x y] velocity delta]
  [(+ x (* delta velocity))
   y])

(defn end-game!
  []
  ;; (println "game lost!!!")

  )

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
          ;; (println "entity" new-entity "collided with players:" collided-players)
          (end-game!))
        (replace-entity new-entity)))))

(defn entity-positions
  []
  (doseq [entity (find-entities-by-component :positionable)]
    (println "Entity with name" (:name entity)
             "has position:" (:position (utils/find-component :positionable entity)))))

(defn entity-dimentions
  [e]
  (let [boundable (utils/find-component :boundable e)
        positionable (utils/find-component :positionable e)
        [px py] (:position positionable)
        bounds (:bounds boundable)
        canvas (:canvas @game-state)
        width (.-width canvas)
        height (.-height canvas)
        [w h] (absolute-bounds bounds width height)
        [x y] [(int (* width (/ px 100)))
               (int (* height (/ py 100)))]]
    [x y w h]))

(defn get-entities
  []
  (println (:entities @game-state)))

(defn print-entities
  []
  (doseq [entity (sort-by z-order
                          (:entities @game-state))]
    (println entity)))

(defn keyboard-handler
  [event]
  (let [action (get keyboard-controls (.-keyCode event))]
    (when action
      (condp = action
        :left (move-entity! :player (- move-speed) 0)
        :right (move-entity! :player move-speed 0)
        :up (move-entity! :player 0 (- move-speed))
        :down (move-entity! :player 0 move-speed)
        nil))))

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
  (doseq [entity (find-entities-by-component :weaponised)]
    (let [weaponised (utils/find-component :weaponised entity)
          laser (:laser weaponised)
          interval (:every laser)
          last-time (:last laser)
          now (js/Date.)]
      (when (or (nil? last-time)
                (>= (.getTime now) (+ (.getTime last-time) interval)))
        (generate-laser now entity)))))

(defn render
  [delta]
  (lasers-zomg! delta)
  (move-entities delta)
  (cleanup-entities)
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
  (let [body (utils/get-body-elem)]
    (utils/set-element-style body (:body styles)))
  (utils/set-element-style canvas (:canvas styles)))

;; events->chan on keyboard -> println the event (check it's not duplicated)

(defn boot-game
  []
  (let [canvas (game-surface)
        _ (println "at boot, canvas is:" canvas)
        surface (.getContext canvas "2d")
        _ (println "at boot, 2d surface is:" surface)
        ;; TODO: listening on js/window might break LD pages!!!
        [keyboard-event-key kbd-chan] (utils/events->chan js/window
                                                          EventType.KEYDOWN
                                                          (chan 1))]
    (swap! game-state assoc :canvas canvas :surface surface)
    (setup-canvas canvas)
    (setup-styles canvas)
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
          rafchan (do (render v)
                      (recur)))))))

(defn setup []
  (upgrade-game-state)
  (boot-game))

(defn teardown []
  (when control-chan
    (println "Sending close signal")
    (go (>! control-chan :close))
    ;; (reset! control-chan (chan))
    )
  ;; While game-state is still in flux...
  (reset! game-state {})
  ;; (when @rafID
  ;;   (js/window cancelAnimationFrame @rafID))
  )

(when-not @game-booted
  (boot-game)
  (reset! game-booted :yup))

(fw/start {
           ;; configure a websocket url if you are using your own server
           ;; :websocket-url "ws://localhost:3449/figwheel-ws"

           ;; optional callback
           :on-jsload (fn []
                        (teardown)
                        (setup))

           ;; The heads up display is enabled by default
           ;; to disable it:
           ;; :heads-up-display false

           ;; when the compiler emits warnings figwheel
           ;; blocks the loading of files.
           ;; To disable this behavior:
           ;; :load-warninged-code true

           ;; if figwheel is watching more than one build
           ;; it can be helpful to specify a build id for
           ;; the client to focus on
           ;; :build-id "example"
           })
