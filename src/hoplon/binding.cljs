(ns hoplon.binding
  (:refer-clojure :exclude [binding])
  (:require-macros [hoplon.binding :as b]))

(def ^:export tmp    "Temporary variable for eval." nil)
(def thread-bindings "Stack of binding maps."       (atom []))
(def global-bindings "Map of initial var bindings." (atom {}))

(defn push-thread-bindings
  "Given a map with munged js variable names (as strings) for keys and the
  binding values as values, sets the variables to their new values and adds
  the binding map to the thread-bindings stack. If there are aren't yet any
  bindings for a variable its current value is stored in the global-bindings
  map so it can be restored later."
  [binding-map]
  (let [current (apply merge @thread-bindings)]
    (swap! thread-bindings conj binding-map)
    (doseq [[k v] binding-map]
      (when-not (contains? current k)
        (swap! global-bindings assoc k (js* "eval(~{})" k)))
      (set! hoplon.binding.tmp v)
      (js* "eval(~{})" (str k " = hoplon.binding.tmp" )))))

(defn pop-thread-bindings
  "Pops the topmost binding map from thread-bindings stack and restores the
  variables to their previous saved states."
  []
  (let [popped  (peek @thread-bindings)
        current (apply merge (swap! thread-bindings pop))]
    (doseq [k (keys popped)]
      (set! hoplon.binding.tmp (get current k (get @global-bindings k)))
      (js* "eval(~{})" (str k " = hoplon.binding.tmp" )))))

(defn bound-fn*
  "Given a function f, returns a new function capturing the current bindings
  in its closure. When the returned function is invoked the saved bindings
  are pushed and set, f is applied to the arguments, and bindings are restored
  to their previous values."
  [f]
  (let [binding-map (apply merge @thread-bindings)]
    (fn [& args]
      (push-thread-bindings binding-map)
      (try (apply f args) (finally (pop-thread-bindings))))))
