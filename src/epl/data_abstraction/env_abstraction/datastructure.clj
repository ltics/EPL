(ns epl.data-abstraction.env-abstraction.datastructure
  (:require [epl.cota :refer :all]))

(def report-no-binding-found
  (lambda [search-var]
    (prn (str 'apply-env "No binding for ~s" search-var))))

(def report-invalid-env
  (lambda [env]
    (prn (str 'apply-env "Bad environment: ~s" env))))

;; data definition:
;; Env ::= (empty-env) | (extend-env Var Schemeval Env)

;; empty-env : () -> Env
(def empty-env
  (lambda [] (list 'empty-env)))

;; extend-env : Var * Schemeval * Env -> Env
(def extend-env
  (lambda [var val env]
    (list 'extend-env var val env)))

;; apply-env : Env * Var -> Schemeval
(def apply-env
  (lambda [env search-var]
    (cond (= (car env) 'empty-env) (report-no-binding-found search-var)
          (= (car env) 'extend-env) (let [saved-var (cadr env)
                                          saved-val (caddr env)
                                          saved-env (cadddr env)]
                                      (if (= search-var saved-var)
                                        saved-val
                                        (apply-env saved-env search-var)))
          :else (report-invalid-env env))))