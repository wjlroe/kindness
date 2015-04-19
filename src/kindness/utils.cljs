(ns kindness.utils
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as async
             :refer [>! <! put! chan alts! timeout]]
            [goog.style :as style]
            [goog.dom :as dom]
            [goog.events :as events])
  (:import [goog.async AnimationDelay]
           [goog.events EventType]))

(defn guid []
  (let [s4 (fn [] (-> (Math/floor (* (inc (Math/random)) 0x10000))
                   (.toString 16)
                   (.substring 1)))]
    (str (s4) (s4) "-" (s4) "-" (s4) "-" (s4) "-" (s4) (s4) (s4))))

(defn raf-to-chan
  "Turn an animation loop based on requestAnimationFrame into a channel.

   TODO: Throw away large time intervals which indicate the game has
   gone into the background and then back again."
  []
  (let [raf (chan)
        internal (chan)]
    (let [ad (AnimationDelay. (fn [time] (go (>! internal time))))]
      (go-loop [previous nil]
        (.start ad)
        (let [time (<! internal)
              delta (- time (or previous time))]
          (>! raf delta)
          (recur time))))
    raf))

(defn get-body-elem
  []
  (first (array-seq (.getElementsByTagName js/document "body"))))

(defn create-element-if-not-exist
  [id node-name]
  (let [node (.getElementById js/document id)]
    (if node
      node
      (let [elem (.createElement js/document node-name)
            body (get-body-elem)]
        (set! (.-id elem) id)
        (.appendChild body elem)
        elem))))

(defn set-element-style
  [element style]
  (doseq [[key value] style]
    (style/setStyle element (name key) value)
    ))

(defn insert-image-assets!
  [images]
  (when-not (dom/getElement "images")
    (let [body (get-body-elem)
          div (dom/createElement "div")]
      (set! (.-id div) "images")
      (set-element-style div {:display "none"})
      (doseq [[id filename] images]
        (let [img (dom/createElement "img")]
          (set! (.-id img) (name id))
          (set! (.-src img) (str "images/" filename))
          (dom/appendChild div img)))
      (dom/appendChild body div))))

(defn find-image
  [id]
  (dom/getElement (name id)))

(defn events->chan
  "Given a target DOM element and event type return a channel of
  observed events. Can supply the channel to receive events as third
  optional argument.

   Return the event goog.events.Key for the listener and the channel."
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
   (println "events->chan. el:" el "event-type:" event-type)
   [(events/listen
     el
     event-type
     (fn [e] (put! c e)))
    c]))

(defn stop-events
  [event-key]
  (println "Stopping events:" )
  (println event-key)
  (events/unlistenByKey event-key)
  (println "Stopped"))

(defn find-component [component entity]
  (get (:components entity) component))

(defn position-and-bounds
  [entity]
  (let [boundable (find-component :boundable entity)
        positionable (find-component :positionable entity)
        [x y] (:position positionable)
        [w h] (:bounds boundable)]
    [x y w h]))
