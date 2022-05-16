(ns linter.core
  (:require [cljs.main]
            [cljs.analyzer.api :as ana-api]
            [cljs.analyzer :as ana]
            [linter.impl]
            [linter.plugins.react-hooks.core :as react-hooks]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn linter-pass [{:keys [plugins]}]
  (fn [env ast opts]
    (doseq [{:keys [lint-fn] :as config} plugins]
      (binding [linter.impl/*config* config]
        (doseq [[error-type env opts] (lint-fn ast)]
          (ana/warning error-type env opts))))
    ast))

(defn build-cljs-main [linter-pass warnings build-fn]
  (let [passes (into ana-api/default-passes [linter-pass])]
    (ana-api/with-passes passes
      (binding [ana/*cljs-warnings* warnings]
        (build-fn)))))

(defn build-shadow-cljs [linter-pass warnings build-fn]
  (require 'shadow.build.api)
  (if-let [init (resolve 'shadow.build.api/init)]
    (do
      (alter-var-root init (fn [f] #(update (f) :analyzer-passes conj linter-pass)))
      (binding [ana/*cljs-warnings* warnings]
        (build-fn)))
    (throw (AssertionError. "attempted to run linter with shadow-cljs, but couldn't find shadow in deps"))))

(def default-plugins
  {:react/hooks {:lint-fn #'react-hooks/lint-hooks :warnings react-hooks/warnings}})

(defn build [{:keys [build-tool plugins] :as config} build-fn]
  (let [plugins (map (fn [[plugin-name plugin-config]]
                       (merge (default-plugins plugin-name) plugin-config))
                     plugins)
        warnings (apply merge ana/*cljs-warnings* (map :warnings plugins))
        linter-pass (linter-pass (assoc config :plugins plugins))]
    (case build-tool
      :cljs.main (build-cljs-main linter-pass warnings build-fn)
      :shadow-cljs (build-shadow-cljs linter-pass warnings build-fn))))

(defmacro with-linter [{:keys [build-tool plugins] :as config} & body]
  `(build ~config (fn [] ~@body)))

(comment
  (future
    (with-linter
      {:build-tool :cljs.main
       :plugins {:react/hooks {}}}
      (cljs.main/-main "-w" "dev" "-c" "linter.test"))))

