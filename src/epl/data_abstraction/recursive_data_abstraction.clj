(ns epl.data-abstraction.recursive-data-abstraction
  (:require [epl.cota :refer :all]))

;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

;; var-exp : Var -> Lc-exp
(def var-exp
  (lambda [var]
    '(var-exp var)))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(def lambda-exp
  (lambda [var lc-exp]
    '(lambda-exp var lc-exp)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(def app-exp
  (lambda [lc-exp1 lc-exp2]
    '(app-exp lc-exp1 lc-exp2)))

;; var-exp? : Lc-exp -> Bool
(def var-exp?
  (lambda [x]
    (and (list? x) (= (car x) 'var-exp))))

;; lambda-exp? : Lc-exp -> Bool
(def lambda-exp?
  (lambda [x]
    (and (list? x) (= (car x) 'lambda-exp))))

;; app-exp? : Lc-exp -> Bool
(def app-exp?
  (lambda [x]
    (and (list? x) (= (car x) 'app-exp))))

;; var-exp->var : Lc-exp -> Var
(def var-exp->var
  (lambda [x]
    (cadr x)))

;; lambda-exp->bound-var : Lc-exp -> Var
(def lambda-exp->bound-var
  (lambda [x]
    (cadr x)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(def lambda-exp->body
  (lambda [x]
    (caddr x)))

;; app-exp->rator : Lc-exp -> Lc-exp
(def app-exp->rator
  (lambda [x]
    (cadr x)))

;; app-exp->rand : Lc-exp -> Lc-exp
(def app-exp->rand
  (lambda [x]
    (caddr x)))

;; occurs-free? : Sym * Lcexp -> Bool
(def occurs-free?
  (lambda [search-var exp]
    (cond (var-exp? exp) (= search-var (var-exp->var exp))
          (lambda-exp? exp) (and
                              (not (= search-var (lambda-exp->bound-var exp)))
                              (occurs-free? search-var (lambda-exp->body exp)))
          :else (or
                  (occurs-free? search-var (app-exp->rator exp))
                  (occurs-free? search-var (app-exp->rand exp))))))
