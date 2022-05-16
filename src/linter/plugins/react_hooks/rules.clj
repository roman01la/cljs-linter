(ns linter.plugins.react-hooks.rules
  (:require [clojure.set :as set]
            [linter.plugins.react-hooks.impl :as impl]))

(defn infer-deps-type [ast]
  (or (-> ast :fn :info :meta :deps/type)
      :js-array))

(defn valid-deps-type? [ast]
  (let [deps (impl/hook->deps ast)]
    (= (infer-deps-type ast) (:op deps))))

(defn lint-deps [ast errors]
  (let [non-inline-fn (impl/hook->non-inline-fn ast)
        deps (impl/hook->deps ast)
        deps-type (infer-deps-type ast)
        deps-array? (and (some? deps) (valid-deps-type? ast))
        not-deps-array? (and (some? deps) (not (valid-deps-type? ast)))
        literal-value-in-deps (when deps-array?
                                (impl/hook->literal-values-in-deps ast))
        can-check-deps? (and (not non-inline-fn) deps-array?)
        ;; all-missing-deps = in-body locals - declared deps
        all-missing-deps (when can-check-deps?
                           (set (impl/hook->missing-deps ast)))
        ;; all-unnecessary-deps = stable deps - (in-body locals + declared deps)
        all-unnecessary-deps (when can-check-deps?
                               (set (impl/hook->unnecessary-deps ast)))
        ;; missing-deps = all-missing-deps - all-unnecessary-deps
        missing-deps (set/difference all-missing-deps all-unnecessary-deps)
        ;; unnecessary-deps = all-unnecessary-deps - all-missing-deps
        unnecessary-deps (set/difference all-unnecessary-deps (set (impl/hook->used-locals ast)))
        suggested-deps (set/difference (set (impl/hook->used-locals ast)) all-unnecessary-deps)]
    (cond-> errors
            non-inline-fn (conj [::non-inline-fn (:env non-inline-fn) {:node non-inline-fn}])
            not-deps-array? (conj [::not-array-deps (:env deps) {:node deps :deps-type deps-type}])
            (seq literal-value-in-deps) (conj [::literal-value-in-deps (:env deps) {:literals literal-value-in-deps
                                                                                    :deps-type deps-type}])
            (seq missing-deps) (conj [::missing-deps (:env deps) {:missing-deps missing-deps
                                                                  :unnecessary-deps unnecessary-deps
                                                                  :suggested-deps suggested-deps
                                                                  :deps-type deps-type}]))))

(defn lint-call-location
  "Checks if a hook is called in prohibited locations:
  - in a non-hook fn (an anonymous fn or a function whose name doesn't start with `use`)
  - at top level (outside of function scope)
  - in a branch
  - in a loop"
  [ast errors]
  (let [hook-call-in-fn (impl/hook-call-in-fn ast)]
    ;; TODO: priority, ::hook-call-at-top - ::hook-call-in-loop - ::hook-call-in-branch - ::hook-call-in-fn
    (cond-> errors
            hook-call-in-fn (conj [::hook-call-in-fn (:env ast) {:node ast :fn-scope hook-call-in-fn}])
            (impl/hook-call-at-top? ast) (conj [::hook-call-at-top (:env ast) {:node ast}])
            (impl/hook-call-in-branch? ast) (conj [::hook-call-in-branch (:env ast) {:node ast}])
            (impl/hook-call-in-loop? ast) (conj [::hook-call-in-loop (:env ast) {:node ast}]))))

(defn lint-unsafe-calls [ast errors]
  (let [deps-type (infer-deps-type (impl/hook->deps ast))
        unsafe-set-state-calls (impl/hook->unsafe-set-state-calls ast)]
    (cond-> errors
            (seq unsafe-set-state-calls)
            (into (map (fn [ast] [::unsafe-set-state-call (:env ast) {:node ast :deps-type deps-type}]) unsafe-set-state-calls)))))
