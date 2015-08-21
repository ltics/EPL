(ns epl.data-abstraction.env-abstraction.procedural
  (:require [epl.cota :refer :all]))

(def report-no-binding-found
  (lambda [search-var]
    (prn (str 'apply-env "No binding for ~s" search-var))))

;; data definition:
;; Env = Var -> Schemeval

;; empty-env : () -> Env
(def empty-env
  (lambda []
    (lambda [search-var]
      (report-no-binding-found search-var))))

(declare apply-env)

(def extend-env
  (lambda [saved-var saved-val saved-env]
    (lambda [search-var]
      (if (= search-var saved-var)
        saved-val
        (apply-env saved-env search-var)))))

(def apply-env
  (lambda [env search-var]
    (env search-var)))