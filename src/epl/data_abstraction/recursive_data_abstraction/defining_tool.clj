(ns epl.data-abstraction.recursive-data-abstraction.defining-tool
  (:require [epl.cota :refer :all]
            [epl.corelib :refer :all]
            [clojure.test :refer :all]))

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
                                           (app-exp (var-exp 'y) (var-exp 'z)))))))))

(run-tests)
