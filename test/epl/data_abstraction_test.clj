(ns epl.data-abstraction-test
  (:require [clojure.test :refer :all]
            [epl.cota :refer :all]
            [epl.data-abstraction.numeral-abstraction :refer :all]
            [epl.data-abstraction.env-abstraction.datastructure :as eds]
            [epl.data-abstraction.env-abstraction.procedural :as eproc]
            [epl.data-abstraction.recursive-data-abstraction.interfaces :as ri]))

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

(deftest env-abstraction
  (testing "datastructure"
    (let [env (eds/extend-env 'd 6
                (eds/extend-env 'y 8
                  (eds/extend-env 'x 7
                    (eds/extend-env 'y 14
                      (eds/empty-env)))))]
      (is= (eds/apply-env env 'd) 6)
      (is= (eds/apply-env env 'y) 8)
      (is= (eds/apply-env env 'x) 7)))
  (testing "procedural"
    (let [env (eproc/extend-env 'd 6
                (eproc/extend-env 'y 8
                  (eproc/extend-env 'x 7
                    (eproc/extend-env 'y 14
                      (eproc/empty-env)))))]
      (is= (eproc/apply-env env 'd) 6)
      (is= (eproc/apply-env env 'y) 8)
      (is= (eproc/apply-env env 'x) 7))))

(deftest recursive-data-abstraction
  (testing "interfaces"
    (is (false? (ri/occurs-free? 'a (ri/lambda-exp 'a (ri/app-exp (ri/var-exp 'b) (ri/var-exp 'a))))))
    (is (true? (ri/occurs-free? 'b (ri/lambda-exp 'a (ri/app-exp (ri/var-exp 'b) (ri/var-exp 'a))))))
    (is (ri/occurs-free? 'x (ri/var-exp 'x)))
    (isnot (ri/occurs-free? 'x (ri/var-exp 'y)))
    (isnot (ri/occurs-free? 'x (ri/lambda-exp 'x (ri/app-exp (ri/var-exp 'x) (ri/var-exp 'y)))))
    (is (ri/occurs-free? 'x (ri/lambda-exp 'y (ri/app-exp (ri/var-exp 'x) (ri/var-exp 'y)))))
    (is (ri/occurs-free? 'x (ri/app-exp (ri/lambda-exp 'x (ri/var-exp 'x)) (ri/app-exp (ri/var-exp 'x) (ri/var-exp 'y)))))
    (is (ri/occurs-free? 'x (ri/lambda-exp 'y (ri/lambda-exp 'z (ri/app-exp (ri/var-exp 'x) (ri/app-exp (ri/var-exp 'y) (ri/var-exp 'z)))))))))