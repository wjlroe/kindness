(ns kindness.gamepad)

(defn raw-gamepads
  []
  (cond (.-getGamepads js/navigator)
        (.getGamepads js/navigator)
        (.-webkitGetGamepads js/navigator)
        (.webkitGetGamepads js/navigator)
        :else []))
