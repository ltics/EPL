(ns epl.test_cota
  (:require [clojure.test :refer :all]))

(defmacro is= [& body]
  `(is (= ~@body)))
