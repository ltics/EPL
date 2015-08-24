(ns epl.data-abstraction-test
  (:require [clojure.test :refer :all]
            [epl.cota :refer :all]
            [epl.data-abstraction.numeral-abstraction :refer :all]
            [epl.data-abstraction.env-abstraction.datastructure :as eds]
            [epl.data-abstraction.env-abstraction.procedural :as eproc]))

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
