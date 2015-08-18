(ns epl.inductive_data_test
  (:require [clojure.test :refer :all]
            [epl.test_cota :refer :all]
            [epl.inductive-data :refer :all]))

(deftest inductive-data
  (testing "in-S?"
    (is (true? (in-S? 3)))
    (is (false? (in-S? 4))))
  (testing "list-length"
    (is= (list-length '(1 2 3)) 3))
  (testing "nth-element"
    (is= (nth-element '(1 2 3) 0))
    (is (nil? (nth-element '(1 2 3) 3))))
  (testing "remove-first"
    (is= (remove-first 'a '(a b c)) '(b c))
    (is= (remove-first 'b '(e f g)) '(e f g))
    (is= (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
    (is= (remove-first 'x '()) '())))
