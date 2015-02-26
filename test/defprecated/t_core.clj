(ns defprecated.t-core
  (:require [midje.sweet :refer :all]
            [defprecated.core :as depr])
  (:import java.io.StringWriter))

(defmacro return-output-stream [& body]
  `(let [sw# (StringWriter.)]
     (binding [*out* sw#]
       ~@body)
     (str sw#)))

(facts "about def"
  (fact "depr/def just attaches deprecation notice to docs"
    (depr/def var1 "Sample doc." 0)
    (:doc (meta #'var1))
    => #(.startsWith % "WARNING: #'defprecated.t-core/var1 is deprecated."))
  (fact "original :deprecated meta is preserved"
    (depr/def ^{:deprecated "1.2"} var2)
    (:deprecated (meta #'var2)) => "1.2"

    (depr/def ^{:deprecated {:in "1.3"}} var3)
    (:deprecated (meta #'var3)) => "1.3")
  (fact ":use-instead can be used"
    (depr/def ^{:deprecated {:use-instead filter}
                :doc "Sample doc."} var4)
    (:doc (meta #'var4))
    => #(.startsWith % "WARNING: #'defprecated.t-core/var4 is deprecated, use #'clojure.core/filter instead.")))

(depr/defn fun4 {:deprecated {:print-warning :always}}
  ([x] x)
  (^:deprecated [x y] (+ x y)))

(facts "about defn"
  (depr/defn fun1 [] 42)
  (fact "has deprecation notice in docstring"
    (:doc (meta #'fun1))
    => #(.startsWith % "WARNING: #'defprecated.t-core/fun1 is deprecated."))
  (fact "prints deprecation warning once when called"
    (return-output-stream (fun1) (fun1))
    => "WARNING: #'defprecated.t-core/fun1 is deprecated.\n")
  (fact ":print-warning :always prints warnings every time"
    (depr/defn fun2 {:deprecated {:print-warning :always}} [] 42)
    (return-output-stream (fun2) (fun2))
    => "WARNING: #'defprecated.t-core/fun2 is deprecated.\nWARNING: #'defprecated.t-core/fun2 is deprecated.\n")
  (fact ":print-warning :never suppresses warnings"
    (depr/defn fun3 {:deprecated {:print-warning :never}} [] 42)
    (return-output-stream (fun3))
    => "")

  (fact "functions with deprecated arities are not marked as deprecated themselves"
    (:doc (meta #'fun4)) =not=> #(.startsWith % "WARNING"))
  (fact ":forms metadata contains only non-deprecated arities"
    (:forms (meta #'fun4)) => '([x]))
  (fact "only deprecated arities print a warning"
    (return-output-stream (fun4 4))
    => ""
    (return-output-stream (fun4 4 5))
    => "WARNING: #'defprecated.t-core/fun4 arity [x y] is deprecated, use one of ([x]) instead.\n")

  (fact ":print-function allows to customize how to output the warning"
    (def out (atom []))
    (depr/defn fun5 {:deprecated {:print-function (fn [s] (swap! out conj s))
                                  :print-warning :once}}
      [] 42)
    (return-output-stream (fun5)) => ""
    @out => ["WARNING: #'defprecated.t-core/fun5 is deprecated."]))









