(ns linter.plugins.react-hooks.impl
  (:require [cljs.analyzer.passes :as passes]))

(defn hook? [sym]
  (and (symbol? sym)
       (some? (re-find #"^use-|use[A-Z]" (name sym)))))

(defn hook-def? [ast]
  (and (= :def (:op ast))
       (-> ast :var :name hook?)))

(defn hook-call? [ast]
  (and (= :invoke (:op ast))
       (-> ast :fn :name hook?)))

(defn hook-call-in-fn [ast]
  (when-let [fn-scope (some-> ast :env :fn-scope last)]
    (let [fn-name (:name fn-scope)
          hook? (and fn-name (hook? fn-name))
          component? false]
      (when (and (not hook?) (not component?))
        {:fn-name fn-name}))))

(defn hook-call-at-top? [ast]
  (-> ast :env :fn-scope empty?))

(defn hook-call-in-branch? [ast]
  (-> ast :env ::in-branch))

(defn hook-call-in-loop? [ast]
  (-> ast :env :in-loop))

(def known-hooks-with-deps
  '#{use-effect useEffect
     use-layout-effect useLayoutEffect
     use-callback useCallback
     use-memo useMemo
     use-imperative-handle useImperativeHandle})

(defn known-hook-with-deps? [ast]
  (-> ast :fn :name name symbol known-hooks-with-deps))

(defn hook->non-inline-fn [ast]
  (let [fn-arg (-> ast :args first)]
    (when (not= :fn (:op fn-arg))
      fn-arg)))

(defn js-array? [ast]
  (= :js-array (:op ast)))

(defn hook->deps [ast]
  (some-> ast :args second))

(def const-types
  '#{clj-nil number string boolean 'cljs.core/Symbol 'cljs.core/Keyword})

(defn const? [ast]
  (and (= :const (:op ast))
       (const-types (:tag ast))))

(defn hook-call->deps [ast]
  (-> ast :args second :items))

(defn hook->literal-values-in-deps [ast]
  (filter const? (hook-call->deps ast)))

(defn hook->body-nodes [pred getfn ast]
  (let [used-locals (atom [])]
    (-> (first (:args ast))
        (passes/walk [(fn [env ast opts]
                        (when (pred ast)
                          (swap! used-locals conj (getfn ast)))
                        ast)]))
    @used-locals))

(defn hook->used-locals [ast]
  (hook->body-nodes #(= :local (:op %)) :info ast))

(defn hook->missing-deps
  "used-locals - declared deps"
  [ast]
  (let [deps (into #{} (map :info) (hook-call->deps ast))
        used-locals (hook->used-locals ast)
        missing-deps (remove deps used-locals)]
    missing-deps))

(def stable-hooks
  '#{use-state useState
     use-reducer useReducer
     use-ref useRef
     use-event useEvent})

(defn stable-hook-fn?
  "Returns `true` when `ast` is coming from a hook

  (let [ref (use-ref)]
    ref) ;; ast"
  [ast]
  (and (hook-call? (:init ast))
       (stable-hooks (-> ast :init :fn :name name symbol))))

(defn hook-destructured-fn?
  "Returns `true` when `ast` is coming from a hook

  (let [[state set-state] (use-state 0)]
    (set-state)) ;; ast"
  [ast]
  (and (= 'cljs.core/nth (-> ast :init :fn :name))
       (= 1 (-> ast :init :args second :val))
       (hook-call? (-> ast :init :args first :init))))

(defn stable-hook-destructured-fn?
  "Returns `true` when `ast` is a reference to a stable local
  coming from use-state or use-reducer hooks"
  [ast]
  (and (hook-destructured-fn? ast)
       (stable-hooks (-> ast :init :args first :init :fn :name name symbol))))

(defn hook->unnecessary-deps
  "Lookup up stable locals (ref or set-state) in declared deps + used-locals"
  [ast]
  (let [deps (hook-call->deps ast)
        used-locals (hook->used-locals ast)]
    (into []
          (comp (filter (some-fn stable-hook-fn? stable-hook-destructured-fn?))
                (distinct))
          (concat deps used-locals))))

(def state-hooks
  '#{use-state useState
     use-reducer useReducer})

(defn set-state-hook-fn? [ast]
  (or
    (and (hook-destructured-fn? ast)
         (state-hooks (-> ast :init :args first :init :fn :name name symbol)))
    (and (hook-call? (:init ast))
         (state-hooks (-> ast :init :fn :name name symbol)))))

(def effect-hooks
  '#{use-effect useEffect
     use-layout-effect useLayoutEffect})

(defn invoked-local?
  "Returns `true` when `ast` is an invoked local var

  (let [x ...]
    (x)) ;; invoked local var"
  [ast]
  (and (= :invoke (:op ast))
       (= :local (-> ast :fn :op))))

(defn hook->unsafe-set-state-calls
  "Finds unsafe set-state calls in effect hooks without deps that causing infinite loop of updates"
  [ast]
  (when (and (effect-hooks (-> ast :fn :name name symbol))
             (not (hook->deps ast)))
    (->> (hook->body-nodes invoked-local? :fn ast)
         (filter set-state-hook-fn?))))
