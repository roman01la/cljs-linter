(ns linter.plugins.react-hooks.core
  (:require [linter.plugins.react-hooks.error-messages]
            [linter.plugins.react-hooks.rules :as rules]
            [linter.plugins.react-hooks.impl :as impl]
            [linter.impl]))

(def deps-warnings
  {::rules/missing-deps true
   ::rules/literal-value-in-deps true
   ::rules/not-array-deps true
   ::rules/non-inline-fn true})

(def call-site-warnings
  {::rules/hook-call-in-loop true
   ::rules/hook-call-in-branch true
   ::rules/hook-call-at-top true
   ::rules/hook-call-in-fn true})

(def unsafe-calls-warnings
  {::rules/unsafe-set-state-call true})

(def warnings
  (merge deps-warnings call-site-warnings unsafe-calls-warnings))


(defn lint-hooks [ast]
  (when (impl/hook-call? ast)
    (let [errors (cond->> []
                          :always (rules/lint-call-location ast)
                          (impl/known-hook-with-deps? ast) (rules/lint-deps ast)
                          :always (rules/lint-unsafe-calls ast))
          disabled-warnings (when-let [config (:resolve linter.impl/*config*)]
                              (some-> ast :fn :name config :disabled-warnings))]
      (if disabled-warnings
        (remove (comp disabled-warnings first) errors)
        errors))))
