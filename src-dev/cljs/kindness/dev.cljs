(ns kindness.dev
  (:require [figwheel.client :as fw]
            [cljs.core.async :refer [put!]]
            [kindness.game :as game]))

(defn setup []
  (game/upgrade-game-state)
  (game/boot-game))

(defn teardown []
  (when game/control-chan
    (println "Sending close signal")
    (put! game/control-chan :close)))

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
