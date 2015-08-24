(ns epl.cota-test
  (:require [clojure.test :refer :all]
            [epl.cota :refer :all]))

(deftest test-cota
  (testing "define-datatype"
    (do (define-datatype-naive lc-exp
          (var-exp var)
          (lambda-exp bound-var body)
          (app-exp rator rand))
        (is= lc-exp {:app-exp ["rator" "rand"], :lambda-exp ["bound-var" "body"], :var-exp ["var"]}))))
