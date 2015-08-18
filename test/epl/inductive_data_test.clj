(ns epl.inductive_data_test
  (:require [clojure.test :refer :all]
            [epl.test_cota :refer :all]
            [epl.inductive-data :refer :all]))

(deftest inductive-data
  (testing "in-S?"
    (is (true? (in-S? 3)))))
