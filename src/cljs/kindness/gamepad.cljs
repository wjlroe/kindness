(ns kindness.gamepad)

(defn gamepad-list-to-clj
  [gamepadlist]
  (let [num (.-length gamepadlist)]
    (into
     []
     (for [gi (range num)]
       (aget gamepadlist gi)))))

(defn ^:export raw-gamepads
  []
  (cond (.-getGamepads js/navigator)
        (.getGamepads js/navigator)
        (.-webkitGetGamepads js/navigator)
        (.webkitGetGamepads js/navigator)
        :else []))
