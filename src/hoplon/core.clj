;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns hoplon.core
  (:refer-clojure :exclude [subs name])
  (:import [java.util UUID])
  (:require [clojure.walk    :as walk]
            [clojure.string  :as string]
            [javelin.core    :as j]))

(create-ns 'js)

;;-- helpers ----------------------------------------------------------------;;

(defn subs [& args] (try (apply clojure.core/subs args) (catch Throwable _)))
(defn name [& args] (try (apply clojure.core/name args) (catch Throwable _)))

(defn add-doc [docstring pair]
  (if (string? docstring) (list (first pair) docstring (last pair)) pair))

(defn do-def [docstring bindings values]
  (->> (macroexpand `(let [~bindings ~values]))
       (second)
       (walk/postwalk-replace
         {'clojure.lang.PersistentHashMap/create '(partial apply hash-map)})
       (partition 2)
       (map (partial add-doc docstring))
       (map #(cons 'def %))
       (list* 'do)))

(defn parse-e [[tag & [head & tail :as args]]]
  (let [kw1? (comp keyword? first)
        mkkw #(->> (partition 2 %) (take-while kw1?) (map vec))
        drkw #(->> (partition 2 2 [] %) (drop-while kw1?) (mapcat identity))]
    (cond (map?     head) [tag head tail]
          (keyword? head) [tag (into {} (mkkw args)) (drkw args)]
          :else           [tag nil args])))

(defn- map-bind-keys
  [form]
  (when (map? form)
    (->> form
         :keys
         (map (juxt identity #(keyword (name %))))
         (into (dissoc form :keys))
         vals
         (filter keyword?))))

;;-- defining macros --------------------------------------------------------;;

(defmacro def-values
  "Destructuring def, similar to scheme's define-values."
  ([bindings values]
   (do-def nil bindings values))
  ([docstring bindings values]
   (do-def docstring bindings values)))

(defmacro elem
  "Create an anonymous custom element."
  [bind & body]
  `(fn [& args#] (let [~bind (parse-args args#)] ~@body)))

(defmacro defelem
  "Defines an element function.

  An element function creates a DOM Element (parent) given two arguments:

    * `attrs` - a number of key-value pairs for attributes and their values
    * `kids` - a sequence of DOM Elements to be appended/used inside

  The returned DOM Element is itself a function which can accept more
  attributes and child elements."
  [name & forms]
  (let [[_ name [_ & [[bind & body]]]] (macroexpand-1 `(defn ~name ~@forms))]
    `(def ~name (elem ~bind ~@body))))

;;-- caching dom manipulation macros ----------------------------------------;;

(defmacro ^:private safe-deref [expr] `(deref (or ~expr (atom))))

(defmacro loop-tpl
  "Template. Works identically to `for-tpl`, only expects a `:bidings`
  attribute to accomodate the HTML HLisp representation:

    (loop-tpl :bindings [x xs] ...)"
  [& args]
  (let [[_ {[bindings items] :bindings} [body]] (parse-e (cons '_ args))]
    `(loop-tpl* ~items
       (fn [item#] (j/cell-let [~bindings item#] ~body)))))

(defmacro for-tpl
  "Template. Accepts a cell-binding and returns a cell containing a sequence of
  elements:

    (for-tpl [x xs] (span x))

  Must be placed inside another element."
  [[bindings items] body]
  `(loop-tpl* ~items (fn [item#] (j/cell-let [~bindings item#] ~body))))

(defmacro if-tpl
  "Template. Accepts a `predicate` cell and returns a cell containing either
  the element produced by `consequent` or `alternative`, depending on the value
  of the predicate:

    (if-tpl predicate (span \"True\") (span \"False\"))

  Must be placed inside another element."
  [predicate consequent & [alternative]]
  `(let [con# (delay ~consequent)
         alt# (delay ~alternative)
         tpl# (fn [p#] (safe-deref (if p# con# alt#)))]
     ((j/formula tpl#) ~predicate)))

(defmacro when-tpl
  "Template. Accepts a `predicate` cell and returns a cell containing either
  the element produced by `consequent` or nothing, depending on the value of
  the predicate:

    (when-tpl predicate (span \"Value\"))

  Must be placed inside another element."
  [predicate & body]
  `(if-tpl ~predicate (do ~@body)))

(defmacro cond-tpl
  "Template. Accepts a number of `clauses` cell-template pairs and returns a
  cell with the value produced by the matching clause:

    (cond-tpl
      clause-a (span \"A\")
      clause-b (span \"B\")
      :else    (span \"Default\"))

  Must be placed inside another element."
  [& clauses]
  (assert (even? (count clauses)))
  (let [[conds tpls] (apply map vector (partition 2 clauses))
        syms1        (take (count conds) (repeatedly gensym))
        syms2        (take (count conds) (repeatedly gensym))]
    `(let [~@(interleave syms1 (map (fn [x] `(delay ~x)) tpls))
           tpl# (fn [~@syms2] (safe-deref (cond ~@(interleave syms2 syms1))))]
       ((j/formula tpl#) ~@conds))))

(defmacro case-tpl
  "Template. Accepts an `expr` cell and a number of `clauses` and returns a
  cell with the value produced by the matching clause:

    (case-tpl expr
      :a (span \"A\")
      :b (span \"B\")
      (span \"Default\"))

  Must be placed inside another element."
  [expr & clauses]
  (let [[cases tpls] (apply map vector (partition 2 clauses))
        default      (when (odd? (count clauses)) (last clauses))
        syms         (take (inc (count cases)) (repeatedly gensym))]
    `(let [~@(interleave syms (map (fn [x] `(delay ~x)) (conj tpls default)))
           tpl# (fn [expr#] (safe-deref (case expr# ~@(interleave cases syms) ~(last syms))))]
       ((j/formula tpl#) ~expr))))

;;-- various dom macros -----------------------------------------------------;;

(defmacro with-dom
  "Evaluates the body after elem has been inserted into the DOM."
  [elem & body]
  `(when-dom ~elem (fn [] ~@body)))

(defmacro with-timeout
  "Evaluates the body after msec milliseconds, asynchronously. Returns the
  timeout ID which can be used to cancel the operation (see js/clearTimeout)."
  [msec & body]
  `(js/setTimeout (fn [] ~@body) ~msec))

(defmacro with-interval
  "Evaluates the body every msec milliseconds, asynchronously. Returns the
  interval ID which can be used to cancel the operation (see js/clearInterval)."
  [msec & body]
  `(js/setInterval (fn [] ~@body) ~msec))

(defmacro with-init!
  "Evaluates the body after Hoplon has completed constructing the page."
  [& body]
  `(add-initfn! (fn [] ~@body)))

(defmacro with-page-load
  "Evaluates the body when the page is reloaded OR when live-reload reloads."
  [& body]
  `(defonce page-load# (on-page-load (fn [] ~@body))))

(defmacro text
  "Creates a DOM Text node and binds its text content to a formula created via
  string interpolation, so the Text node updates with the formula."
  [form]
  (let [i form]
    (if (string? i)
      `(.createTextNode js/document ~i)
      `(j/with-let [t# (.createTextNode js/document "")]
         (j/cell= (set! (.-nodeValue t#) ~i))))))

;;-- experimental -----------------------------------------------------------;;

(defmacro elem+
  "Experimental."
  [[bind-attr bind-kids] & body]
  (let [attr-keys (map-bind-keys bind-attr)]
    `(fn [& args#]
       (let [[attr# kids#] (parse-args args#)]
         (-> (let [kids*# (j/cell [])
                   attr*# (j/cell ~(zipmap attr-keys (repeat nil)))]
               (j/cell-let [~bind-attr attr*#
                            ~bind-kids (j/cell= (flatten kids*#))]
                 (doto (do ~@body)
                   (set-appendChild! (constantly kids*#))
                   (set-removeChild! (constantly kids*#))
                   (set-setAttribute! (constantly attr*#)))))
             (apply attr# kids#))))))

(defmacro defelem+
  "Experimental.

  Defines an extended element function.

  An extended element function creates a DOM Element given two arguments:

    * `attrs` - a number of key-value pairs for attributes and their values
    * `kids` - a sequence of DOM Elements/Cells producing DOM Elements to be
      appended/used inside

  The returned DOM Element is itself a function which can accept more
  attributes and child elements:

    (defelem+ counter
      [{:keys [state class]} kids]
      (ul :class class, :click #(swap! state (fnil inc 0))
        (loop-tpl :bingings [kid kids]
          (li kid))

  Differences to `defelem`:

    - `kids` argument inside the `defelem+` is a Cell of DOM Elements
      representing any DOM elements supplied during construction or
      appended/removed at a later point in time.
    - `attrs` argument must be destructured as it's also a Cell."
  [name & forms]
  (let [[_ name [_ [bind & body]]] (macroexpand-1 `(defn ~name ~@forms))]
    `(def ~name (elem+ ~bind ~@body))))

(defmacro static
  "Experimental."
  [elem]
  `(let [id# ~(str (gensym "hl"))]
     (or (static-elements id#)
         (~elem :static-id id#))))

(defmacro sexp
  "Experimental."
  [& args]
  (->> (last (parse-e (cons '_ args)))
       (mapcat #(if-not (string? %)
                  [%]
                  (read-string (str "(" % "\n)"))))))

