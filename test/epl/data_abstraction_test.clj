(ns epl.data-abstraction-test
  (:require [clojure.test :refer :all]
            [epl.test_cota :refer :all]
            [epl.data_abstraction.numeral_abstraction :refer :all]))

(deftest numeral-abstraction
  (testing "unary number"
    (is= (normal-int->unary-int 3) '(true true true))
    (is= (unary-int->normal-int '(true true true)) 3)
    (is= (unary-int->normal-int
           (unary-plus
             (normal-int->unary-int 3)
             (normal-int->unary-int 7))) 10))
  (testing "normal number"
    (is= (normal-plus 3 7) 10))
  (testing "reverse number"
    (is= (normal-int->reverse-int 3) (- (- (- 5 5) 5) 5))
    (is=
      (reverse-int->normal-int
        (reverse-plus
          (normal-int->reverse-int 3)
          (normal-int->reverse-int 7))) 10)))
