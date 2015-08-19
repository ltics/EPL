(ns epl.data-abstraction
  (:require [epl.cota :refer :all]))

;; Unary Representation
;; In the unary representation, the natural number n is represented by a list of n #t’s.
;; Thus, 0 is represented by (), 1 is rep- resented by (#t), 2 is represented by (#t #t), etc.
(def unary-zero (lambda [] '()))
(def unary-is-zero? (lambda [n] (null? n)))
(def unary-successor (lambda [n] (cons true n)))
(def unary-predecessor (lambda [n] (cdr n)))

(defn normal-int->unary-int
  [n]
  (if (zero? n)
    (unary-zero)
    (unary-successor (normal-int->unary-int (- n 1)))))

(defn unary-int->normal-int
  [x]
  (if (unary-is-zero? x)
    0
    (+ 1 (unary-int->normal-int (unary-predecessor x)))))

(defn unary-plus
  [x y]
  (if (unary-is-zero? x)
    y
    (unary-successor (unary-plus (unary-predecessor x) y))))

;; Normal Representation
(def normal-zero (lambda [] 0))
(def normal-is-zero? (lambda [n] (zero? n)))
(def normal-successor (lambda [n] (+ n 1)))
(def normal-predecessor (lambda [n] (- n 1)))

(defn normal-plus
  [x y]
  (if (normal-is-zero? x)
    y
    (normal-successor (normal-plus (normal-predecessor x) y))))

;; Reverse Representation
(def reverse-zero (lambda [] 5))
(def reverse-is-zero? (lambda [n] (= n 5)))
(def reverse-successor (lambda [n] (- n 5)))
(def reverse-predecessor (lambda [n] (+ n 5)))

(defn normal-int->reverse-int
  [n]
  (if (zero? n)
    (reverse-zero)
    (reverse-successor (normal-int->reverse-int (- n 1)))))

(defn reverse-int->normal-int
  [x]
  (if (reverse-is-zero? x)
    0
    (+ 1 (reverse-int->normal-int (reverse-predecessor x)))))

(def reverse-plus
  (lambda [x y]
    (if (reverse-is-zero? x)
      y
      (reverse-successor (reverse-plus (reverse-predecessor x) y)))))