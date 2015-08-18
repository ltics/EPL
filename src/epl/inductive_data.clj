(ns epl.inductive-data
  (:require [epl.cota :refer :all]))

;;in-S? : N -> Bool
;;usage: (in-S? n) = #t if n is in S, #f otherwise
(def in-S?
  (lambda [n]
    (if (zero? n)
      true
      (if (>= (- n 3) 0)
        (in-S? (- n 3))
        false))))