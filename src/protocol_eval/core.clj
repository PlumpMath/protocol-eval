(ns protocol-eval.core
  "Implementation of eval using Clojure protocols and pattern matching."
  (:require [clojure.core.match :refer [match]]))


;; Error handling
;; ==============

(defn error
  "Just an easy way to throw exceptions."
  [& args]
  (throw (new Exception (apply str args))))


;; Environment
;; ===========

(defn make-env
  "Creates an environment with bindings (from a Clojure map)
   and a potential parent environment.
   Leave the parent as nil in the top level (global) environment."
  [parent bindings]
  (atom {:parent parent
         :bindings bindings}))

(defn make-empty-env []
  (make-env nil {}))

(defn find-env
  "Find the environment containing a specific symbol.
   Returns the top level environment even if no symbol is found."
  [env-ref sym]
  (if (contains? (:bindings @env-ref) sym)
    env-ref
    (let [parent (:parent @env-ref)]
      (if parent
        (find-env parent sym)
        env-ref))))

(defn lookup
  "Search for the value of a symbol in an environment and its parents."
  [env-ref sym]
  (let [error-sentinel :pieces/sym-not-found
        found-env (find-env env-ref sym)
        answer (get (:bindings @found-env) sym error-sentinel)]
    (if (= answer error-sentinel)
      (error "Can't find symbol " sym " in environment.")
      answer)))

(defn set-binding
  "Set the value of a symbol in the environment or its parents
   (depending on where the symbol is first found).
   Creates a new binding for the symbol in the top environment
   if no binding is found."
  [env-ref sym value]
  (let [found-env (find-env env-ref sym)]
    (if found-env
      (swap! found-env assoc-in [:bindings sym] value)
      (error "Can't set " sym " to " value " since it's not in the environment."))))


;; Evaluation
;; ==========

(defprotocol Evalable
  "Dispatch on the type. The magic trick of this implementation."
  (evaluate [exp env]))

(defn call
  "Invoke a function created by make-λ."
  [f args]
  (if (fn? f)
    (f args)
    (error f " is not a function")))

(defn make-λ
  "Creates a lambda."
  [param-names body env]
  (fn [args]
    (let [nr-of-params (count param-names)
          new-env (make-env env (zipmap param-names args))]
      (if (not= nr-of-params (count args))
        (error "Wrong nr of arguments to function: " (count args)))
      (evaluate body new-env))))

(defn evaluate-list
  "Lists are sent to this function for pattern matching and evaluation."
  [exp env]
  (match exp
         (['progn & forms] :seq) (do (doseq [form forms] (evaluate form env)) :progn)
         (['set! sym value] :seq) (set-binding env sym (evaluate value env))
         (['if pred a b] :seq) (if pred a b)
         (['λ param-names body] :seq) (make-λ param-names body env)
         ([f & args] :seq) (call (evaluate f env) (map #(evaluate % env) args))
         :else (error "Can't match expression " exp)))

(extend-protocol Evalable
  java.lang.Long
  (evaluate [exp env] exp)
  java.lang.String
  (evaluate [exp env] exp)
  clojure.lang.Symbol
  (evaluate [exp env] (lookup env exp))
  clojure.lang.PersistentList
  (evaluate [exp env] (evaluate-list exp env)))


;; A small evaluator object
;; ========================

(def built-in-functions {'+ #(apply + %)
                         '* #(apply * %)
                         '- #(apply - %)
                         '/ #(apply / %)
                         'add #(apply + %)
                         'prn prn})

(defn make-evaluator
  "Returns a function that works as an evaluator,
   encapsulating its own stateful environment"
  []
  (let [env-ref (make-env nil built-in-functions)]
    (fn [exp]
      (evaluate exp env-ref))))







(defmacro run [& forms]
  (let [evaluator# (make-evaluator)
        forms-with-evaluator (map #(cons evaluator# %) forms)]
     `([let e evaluator#] ~@forms-with-evaluator)))

(defmacro runn [& s]
  (println s)
  (let [ev# (make-evaluator)]
     (map #(list ev# %) s)))

(macroexpand-1 '(runn 1 2 3))

;(runn (add 2 3))


(let [evaluator (make-evaluator)]
  (evaluator '(set! a 20))
  (evaluator '(prn a)))



(defmacro bleh [s fun]
  (println s)
  (let [f# fun]
    (map #(list f# %) s)))

(macroexpand-1 '(bleh [5 10 30] inc))

;(bleh [5 10 30])

; TODO: Can you see this?

