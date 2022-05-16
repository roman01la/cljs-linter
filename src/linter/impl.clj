(ns linter.impl
  "Extension of cljs.analyzer"
  (:require [cljs.analyzer :as ana]))

(def ^:dynamic *config* nil)

(defmethod ana/parse 'if
  [op env [_ test then else :as form] name _]
  (when (< (count form) 3)
    (throw (#'ana/compile-syntax-error env "Too few arguments to if" 'if)))
  (when (> (count form) 4)
    (throw (#'ana/compile-syntax-error env "Too many arguments to if" 'if)))
  (let [test-expr (ana/disallowing-recur (ana/analyze (assoc env :context :expr) test))
        then-expr (ana/allowing-redef (ana/analyze (#'ana/set-test-induced-tags (assoc env ::in-branch true) test) then))
        else-expr (ana/allowing-redef (ana/analyze (assoc env ::in-branch true) else))]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :unchecked ana/*unchecked-if*
     :children [:test :then :else]}))

(defmethod ana/parse 'case*
  [op env [_ sym tests thens default :as form] name _]
  (assert (symbol? sym) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (assoc env :context :expr)
        v        (ana/disallowing-recur (ana/analyze expr-env sym))
        tests    (mapv #(mapv (fn [t] (ana/analyze (assoc expr-env ::in-branch true) t)) %) tests)
        thens    (mapv #(ana/analyze (assoc env ::in-branch true) %) thens)
        nodes    (mapv (fn [tests then]
                         {:op :case-node
                          ;synthetic node, no :form
                          :env env
                          :tests (mapv (fn [test]
                                         {:op :case-test
                                          :form (:form test)
                                          :env expr-env
                                          :test test
                                          :children [:test]})
                                       tests)
                          :then {:op :case-then
                                 :form (:form then)
                                 :env env
                                 :then then
                                 :children [:then]}
                          :children [:tests :then]})
                       tests
                       thens)
        default  (ana/analyze (assoc env ::in-branch true) default)]
    (assert (every? (fn [t]
                      (or
                        (-> t :info :const)
                        (and (= :const (:op t))
                             ((some-fn number? string? char?) (:form t)))))
                    (apply concat tests))
            "case* tests must be numbers, strings, or constants")
    {:env env :op :case :form form
     :test v :nodes nodes :default default
     :children [:test :nodes :default]}))

(defn fn-name-var [env locals name]
  (if (some? name)
    (let [ns       (-> env :ns :name)
          shadow   (ana/handle-symbol-local name (get locals name))
          shadow   (when (nil? shadow)
                     (get-in env [:js-globals name]))
          fn-scope (:fn-scope env)
          name-var {:name name
                    :op :binding
                    :local :fn
                    :info {:fn-self-name true
                           :fn-scope fn-scope
                           :ns ns
                           :shadow shadow}}
          tag      (-> name meta :tag)
          ret-tag  (when (some? tag)
                     {:ret-tag tag})]
      (merge name-var ret-tag))
    (let [ns (-> env :ns :name)
          fn-scope (:fn-scope env)
          anon-var {:op :fn
                    :info {:fn-scope fn-scope
                           :ns ns}}]
      anon-var)))

(defmethod ana/parse 'fn*
  [op env [_ & args :as form] name _]
  (let [named-fn?    (symbol? (first args))
        [name meths] (if named-fn?
                       [(first args) (next args)]
                       [name (seq args)])
        ;; turn (fn [] ...) into (fn ([]...))
        meths        (if (vector? (first meths))
                       (list meths)
                       meths)
        locals       (:locals env)
        name-var     (fn-name-var env locals name)
        env          (update-in env [:fn-scope] conj name-var)
        locals       (if (and (some? locals)
                              named-fn?)
                       (assoc locals name name-var)
                       locals)
        form-meta    (meta form)
        type         (::type form-meta)
        proto-impl   (::protocol-impl form-meta)
        proto-inline (::protocol-inline form-meta)
        menv         (-> env
                         (cond->
                           (> (count meths) 1)
                           (assoc :context :expr))
                         ;; clear loop flag since method bodies won't be in a loop at first
                         ;; only tracking this to keep track of locals we need to capture
                         (dissoc :in-loop)
                         (merge {:protocol-impl proto-impl
                                 :protocol-inline proto-inline}))
        methods      (map #(ana/disallowing-ns* (#'ana/analyze-fn-method menv locals % type (nil? name))) meths)
        mfa          (transduce (map :fixed-arity) max 0 methods)
        variadic     (boolean (some :variadic? methods))
        locals       (if named-fn?
                       (update-in locals [name] assoc
                                  ;; TODO: can we simplify? - David
                                  :fn-var true
                                  :variadic? variadic
                                  :max-fixed-arity mfa
                                  :method-params (map :params methods))
                       locals)
        methods      (if (some? name)
                       ;; a second pass with knowledge of our function-ness/arity
                       ;; lets us optimize self calls
                       (ana/disallowing-ns* (ana/analyze-fn-methods-pass2 menv locals type meths))
                       (vec methods))
        form         (vary-meta form dissoc ::protocol-impl ::protocol-inline ::type)
        js-doc       (when (true? variadic)
                       "@param {...*} var_args")
        children     (if (some? name-var)
                       [:local :methods]
                       [:methods])
        inferred-ret-tag (let [inferred-tags (map (partial ana/infer-tag env) (map :body methods))]
                           (when (apply = inferred-tags)
                             (first inferred-tags)))
        ast   (merge {:op :fn
                      :env env
                      :form form
                      :name name-var
                      :methods methods
                      :variadic? variadic
                      :tag 'function
                      :inferred-ret-tag inferred-ret-tag
                      :recur-frames ana/*recur-frames*
                      :in-loop (:in-loop env)
                      :loop-lets ana/*loop-lets*
                      :jsdoc [js-doc]
                      :max-fixed-arity mfa
                      :protocol-impl proto-impl
                      :protocol-inline proto-inline
                      :children children}
                     (when (some? name-var)
                       {:local name-var}))]
    (let [variadic-methods (into []
                                 (comp (filter :variadic?) (take 1))
                                 methods)
          variadic-params  (if (pos? (count variadic-methods))
                             (count (:params (nth variadic-methods 0)))
                             0)
          param-counts     (into [] (map (comp count :params)) methods)]
      (when (< 1 (count variadic-methods))
        (ana/warning :multiple-variadic-overloads env {:name name-var}))
      (when (not (or (zero? variadic-params) (== variadic-params (+ 1 mfa))))
        (ana/warning :variadic-max-arity env {:name name-var}))
      (when (not= (distinct param-counts) param-counts)
        (ana/warning :overload-arity env {:name name-var})))
    (ana/analyze-wrap-meta ast)))
