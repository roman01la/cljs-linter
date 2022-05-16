(ns linter.plugins.react-hooks.error-messages
  (:require [linter.plugins.react-hooks.rules :as rules]
            [cljs.analyzer :as ana]
            [clojure.string :as str]))

(defmethod ana/error-message ::rules/missing-deps [_ {:keys [missing-deps unnecessary-deps suggested-deps deps-type]}]
  (let [missing-deps (map :name missing-deps)
        unnecessary-deps (map :name unnecessary-deps)
        suggested-deps (map :name suggested-deps)]
    (str "React Hook has "
         (when (seq missing-deps)
           (str "missing dependencies: [" (str/join " " missing-deps) "]\n"))
         (when (seq unnecessary-deps)
           (str (when (seq missing-deps) "and ")
                "unnecessary dependencies: [" (str/join " " unnecessary-deps) "]\n"
                (->> unnecessary-deps
                     (keep (fn [sym]
                             (case (:hook (meta sym))
                               ("use-ref" "useRef")
                               (str "`" sym "` is an unnecessary dependency because it's a ref that doesn't change")

                               ("use-state" "useState" "use-reducer" "useReducer")
                               (str "`" sym "` is an unnecessary dependency because it's a state updater function with a stable identity")

                               ("use-event" "useEvent")
                               (str "`" sym "` is an unnecessary dependency because it's a function created using useEvent hook that has a stable identity")

                               nil)))
                     (str/join "\n"))
                "\n"))
         "Update the dependencies " (name deps-type) " to be: [" (str/join " " suggested-deps) "]")))

(defmethod ana/error-message ::rules/non-inline-fn [_ {:keys [node]}]
  (str "React Hook received a function whose dependencies "
       "are unknown. Pass an inline function instead."))

(defmethod ana/error-message ::rules/hook-call-at-top [_ {:keys [node]}]
  (str "React Hook " (:form node) " cannot be called at the top level. "
       "React Hooks must be called in a React function component or a custom React Hook function."))

(defmethod ana/error-message ::rules/hook-call-in-fn [_ {:keys [node fn-scope]}]
  (let [fn-name (:fn-name fn-scope)]
    (str "React Hook " (:form node) " is called in "
         (if fn-name
           (str "function " fn-name)
           (str "anonymous function"))
         " that is neither a React function component nor a custom React Hook function.")))

(defmethod ana/error-message ::rules/hook-call-in-branch [_ {:keys [node]}]
  (str "React Hook " (:form node) " is called conditionally.\n"
       "React Hooks must be called in the exact same order in every component render."))

(defmethod ana/error-message ::rules/hook-call-in-loop [_ {:keys [node]}]
  (str "React Hook " (:form node) " may be executed more than once. "
       "Possibly because it is called in a loop. "
       "React Hooks must be called in the exact same order in "
       "every component render."))

(defmethod ana/error-message ::rules/literal-value-in-deps [_ {:keys [literals deps-type]}]
  (let [values (map :val literals)]
    (str "React Hook was passed literal values in dependency " (name deps-type) ": [" (str/join ", " values) "]\n"
         "Those are not valid dependencies because they never change. You can safely remove them.")))

(defmethod ana/error-message ::rules/unsafe-set-state-call [_ {:keys [node deps-type]}]
  (str "React Hook contains a call to `" (:form node) "`.\n"
       "Without " (name deps-type) " of dependencies, this can lead to an infinite chain of updates.\n"
       "To fix this, pass the state value into " (name deps-type) " of dependencies of the hook."))

(defmethod ana/error-message ::rules/not-array-deps [_ {:keys [node deps-type]}]
  (str "React Hook was passed a "
       "dependency list that is not " (name deps-type) " literal. This means we "
       "can’t statically verify whether you've passed the correct dependencies. "
       "Change it to be " (name deps-type) " literal with explicit set of dependencies."))
