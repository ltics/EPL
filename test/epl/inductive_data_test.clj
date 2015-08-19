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
    (is= (remove-first 'x '()) '()))
  (testing "occurs-free?"
    (is (true? (occurs-free? 'x 'x)))
    (is (false? (occurs-free? 'x 'y)))
    (is (false? (occurs-free? 'x '(lambda (x) (x y)))))
    (is (true? (occurs-free? 'x '(lambda (y) (x y)))))
    (is (true? (occurs-free? 'x '((lambda (x) x) (x y)))))
    (is (true? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))))
  (testing "subst"
    (is= (subst 'a 'b '((b c) (b () d))) '((a c) (a () d))))
  (testing "number-elements-from"
    (is= (number-elements-from '(a b c d e) 0) '((0 a) (1 b) (2 c) (3 d) (4 e))))
  (testing "number-elements"
    (is= (number-elements '(a b c d e)) '((0 a) (1 b) (2 c) (3 d) (4 e))))
  (testing "list-sum"
    (is= (list-sum (list 1 2 3 4 5)) 15))
  (testing "partial-vector-sum"
    (is= (partial-vector-sum (vector 1 2 3 4 5) 4) 15))
  (testing "vector-sum"
    (is= (vector-sum (vector 1 2 3 4 5)) 15)))
