(ns linter.test)

(defn use-effect [x f])


(defn use-state [n])

(defn use-yo [x y]
  (let [[n setn] (use-state 0)]
    (use-effect
      (fn []
        (setn x))
      #js [])))
