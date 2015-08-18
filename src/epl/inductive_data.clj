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