(ns defprecated.core
  (:refer-clojure :exclude [defn defmacro])
  (:require [clojure.core :as core]))

(defn- make-warning-msg [sym ns depr-map]
  (format "WARNING: %s is deprecated%s."
          (str "#'" ns "/" sym)
          (if-let [instead (:use-instead depr-map)]
            (format ", use %s instead"
                    (if-let [instead-var (ns-resolve ns instead)]
                      instead-var instead))
            "")))

(core/defmacro def
  "Sames as `def` but defines a deprecated var."
  {:forms '([symbol docstring? init?])}
  [symbol & args]
  (let [[m init?] (if (string? (first args))
                    [{:doc (first args)} (next args)]
                    [{} args])
        m (conj (if (meta symbol) (meta symbol) {}) m)

        depr-map (:deprecated m)
        depr-map (if (map? depr-map) depr-map {:in depr-map})
        m (update-in m [:doc] #(str (make-warning-msg symbol *ns* depr-map)
                                    "\n  " %))
        m (assoc m :deprecated (:in depr-map "current version"))]
    (list* 'def (with-meta symbol m) init?)))

(core/defn- make-invokable
  "Produces code for defining deprecated functions or macros."
  [original name fdecl]
  (let [[m fdecl] (if (string? (first fdecl))
                    [{:doc (first fdecl)} (next fdecl)]
                    [{} fdecl])
        [m fdecl] (if (map? (first fdecl))
                    [(conj m (first fdecl)) (next fdecl)]
                    [m fdecl])
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        [m fdecl] (if (map? (last fdecl))
                    [(conj m (last fdecl)) (butlast fdecl)]
                    [m fdecl])
        m (conj (if (meta name) (meta name) {}) m)

        [good-arities depr-arities] ((juxt remove filter)
                                     #(:deprecated (meta %)) (map first fdecl))
        has-depr-arities (not (empty? depr-arities))
        depr-map (:deprecated m)
        depr-map (conj {:print-warning :once}
                       (if (map? depr-map) depr-map {:in depr-map}))
        warn-msg (make-warning-msg name *ns* depr-map)
        arity-warn-fmt (str "WARNING: arit%s %s %s deprecated, use one of "
                            (seq good-arities) " instead.")
        print-fn (:print-function depr-map 'println)
        warning-sym (gensym "warning")
        m (transient m)]
    (if has-depr-arities
      (do
        (assoc! m :deprecated nil)
        (assoc! m :forms (list `quote good-arities)))
      (do
        (assoc! m :deprecated (:in depr-map "current version"))
        (assoc! m :doc (str warn-msg "\n  " (:doc m)))))
    (case (:print-warning depr-map)
      :always
      (list* original name (persistent! m)
             (for [[params & body] fdecl]
               `(~params
                 ~@(if has-depr-arities
                     (when (:deprecated (meta params))
                       `((~print-fn ~(format arity-warn-fmt "y" params "is"))))
                     `((~print-fn ~warn-msg)))
                 ~@body)))

      :once
      `(let [~warning-sym (delay (~print-fn
                                  ~(if has-depr-arities
                                     (format arity-warn-fmt "ies"
                                             (seq depr-arities) "are")
                                     warn-msg)))]
         ~(list* original name (persistent! m)
                 (for [[params & body] fdecl]
                   `(~params
                     ~@(when (or (not has-depr-arities)
                                 (:deprecated (meta params)))
                         `((force ~warning-sym)))
                     ~@body))))

      :never (list* original name (persistent! m) fdecl))))

(core/defmacro defn
  "Sames as `clojure.core/defn` but defines a deprecated funcion. "
  [name & fdecl]
  (make-invokable 'clojure.core/defn name fdecl))

(core/defmacro defmacro
  "Sames as `clojure.core/defmacro` but defines a deprecated macro. "
  [name & fdecl]
  (make-invokable 'clojure.core/defmacro name fdecl))
