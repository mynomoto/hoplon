(ns hoplon.binding
  (:refer-clojure :exclude [binding bound-fn])
  (:require [clojure.core :as clj]
            [cljs.analyzer :as a]
            [cljs.compiler :as c]))

(defmethod a/error-message :bind/export
  [warning-type info]
  (str (:name info) " not declared ^:export"))

(defn- confirm-export [env var]
  (when-not (:export var)
    (clj/binding [a/*cljs-warnings* (assoc a/*cljs-warnings* :bind/export true)]
      (a/warning :bind/export env var))))

(defmacro binding
  "Like clojure.core/binding, but can be used with bound-fn. The bindings
  should not be ^:dynamic -- they must be ^:export instead."
  [bindings & body]
  (let [env  (assoc &env :ns (a/get-namespace a/*cljs-ns*))
        sets (take-nth 2 (rest bindings))
        vars (->> (take-nth 2 bindings)
                  (map (partial a/resolve-existing-var env)))
        syms (map :name vars)
        gens (take (count vars) (repeatedly gensym))
        jses (map (comp str c/munge) syms)
        lexs (interleave gens sets)
        bmap (zipmap jses gens)
        bind (interleave syms gens)]
    (doseq [v vars] (confirm-export env v))
    `(let [~@lexs]
       (hoplon.binding/push-thread-bindings ~bmap)
       (try ~@body (finally (hoplon.binding/pop-thread-bindings))))))

(defmacro bound-fn
  "Creates a function, capturing the dynamic bindings in place. When the
  function is applied the saved bindings are set before evaluating the body
  and restored after. See clojure.core/bound-fn."
  [args & body]
  `(hoplon.binding/bound-fn* (fn [~@args] ~@body)))
