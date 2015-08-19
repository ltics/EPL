(ns epl.inductive-data
  (:require [epl.cota :refer :all]))

;; in-S? : N -> Bool
;; usage: (in-S? n) = #t if n is in S, #f otherwise
(def in-S?
  (lambda [n]
    (if (zero? n)
      true
      (if (>= (- n 3) 0)
        (in-S? (- n 3))
        false))))

;; list-length : List -> Int
;; usage: (list-length l) = the length of l
(def list-length
  (lambda [lst]
    (if (null? lst)
      0
      (+ 1 (list-length (cdr lst))))))

;; nth-element : List * Int -> SchemeVal
;; usage: (nth-element lst n) = the nth element of lst
(def report-list-too-short
  (lambda [n]
    (prn (str "List too short to get " (+ n 1) "'s elem"))))

(def nth-element
  (lambda [lst n]
    (if (null? lst)
      (report-list-too-short n)
      (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1))))))

;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
(def remove-first
  (lambda [s los]
    (if (null? los)
      '()
      (if (= (car los) s)
        (cdr los)
        (cons (car los) (remove-first s (cdr los)))))))

;; free variable
;; x occurs free in x
;; x occurs free in lambda (y) E if x is not y and x occurs free in E
;; x occurs free in (E F) if x occurs free in E or x occurs free in F

;; occurs-free? : Sym * Lcexp -> Bool
;; usage:
;;   returns #t if the symbol var occurs free in exp,
;;   otherwise returns #f.
(def occurs-free?
  (lambda [var exp]
    (cond (symbol? exp) (= var exp)
          (= (car exp) 'lambda) (and
                                  (not (= var (car (cadr exp))))
                                  (occurs-free? var (caddr exp)))
          :else (or
                  (occurs-free? var (car exp))
                  (occurs-free? var (cadr exp))))))

;; S-list ::=({S-exp}∗)
;; S-exp::=Symbol | S-list
(declare subst-in-s-exp)

;; subst : Sym * Sym * S-list -> S-list
(def subst
  (lambda [new old slist]
    (if (null? slist)
      '()
      (cons
        (subst-in-s-exp new old (car slist))
        (subst new old (cdr slist))))))

;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
(def subst-in-s-exp
  (lambda [new old sexp]
    (if (symbol? sexp)
      (if (= sexp old) new sexp)
      (subst new old sexp))))