(ns epl.data-abstraction.recursive-data-abstraction.defining-tool
  (:require [epl.cota :refer :all]
            [epl.corelib :refer :all]
            [clojure.test :refer :all]))

;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(def occurs-free?
  (lambda [search-var exp]
    (cases lc-exp exp
      (var-exp (var) (= var search-var))
      (lambda-exp (bound-var body)
                  (and (not= search-var bound-var)
                       (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or (occurs-free? search-var rator)
                   (occurs-free? search-var rand))))))

;; S-list ::= ({S-exp}∗)
;; S-exp ::= Symbol | S-list

(declare s-exp?)

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

;; Bintree ::= () | (Int Bintree Bintree)

(defn bintree-to-list
  [bt]
  (cases bintree bt
    (leaf-node (num) `(~'leaf-node ~num))
    (interior-node (key left right)
                   `(~'interior-node ~key
                      ~(bintree-to-list left)
                      ~(bintree-to-list right)))))

;;因为需要define-datatype宏事先创建好一些函数供运行时使用 而如果在其他namespace中进行测试无法强制执行define-datatype lc-exp这段代码 所以只能在当前namespace进行测试
(deftest recursive-data-abstraction
  (testing "defining-tool"
    (is (occurs-free? 'x (var-exp 'x)))
    (isnot (occurs-free? 'x (var-exp 'y)))
    (isnot (occurs-free? 'x
             (lambda-exp 'x
               (app-exp (var-exp 'x) (var-exp 'y)))))
    (is (occurs-free? 'x
          (lambda-exp 'y
            (app-exp (var-exp 'x) (var-exp 'y)))))
    (is (occurs-free? 'x
          (app-exp
            (lambda-exp 'x (var-exp 'x))
            (app-exp (var-exp 'x) (var-exp 'y)))))
    (is (occurs-free? 'x
          (lambda-exp 'y
            (lambda-exp 'z
              (app-exp (var-exp 'x)
                (app-exp (var-exp 'y) (var-exp 'z)))))))
    (is= (empty-s-list) {:variant 'empty-s-list, :type 's-list, :values {}})
    (is (s-list? (empty-s-list)))
    (is (s-exp? (s-list-s-exp (non-empty-s-list 'a (non-empty-s-list 'b (empty-s-list))))))
    (let [tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3))
          tree-2 (interior-node 'bar (leaf-node -1) tree-1)
          tree-3 (interior-node 'baz tree-2 (leaf-node 1))
          tree (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))]
      (is (bintree? tree-1))
      (is (bintree? tree-2))
      (is (bintree? tree-3))
      (is= tree '(interior-node a (leaf-node 3) (leaf-node 4))))))

(run-tests)
