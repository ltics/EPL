(ns epl.corelib
  (:require [epl.cota :refer :all]))

(defmacro data-type-predicate [type-name type-predicate-name]
  `(defn ~type-predicate-name [~'variant]
     (= (:type ~'variant) '~type-name)))

(defmacro data-type-variant [variant type-name]
  (let [variant-name (car variant)
        variant-spec (cdr variant)
        variant-field-names (map car variant-spec)
        ;;对于只有一个元素的列表来进行reverse其实和没有reverse是一样的
        variant-args (mapcat #(reverse (take (if (= (cadr %1) '&) 2 1) %1)) variant-spec)
        variant-field-predicates (map last variant-spec)]
    `(do
       (defn ~(symbol (str variant-name "?")) [data#]
         (and (= (get data# :type) '~type-name)
              (= (get data# :variant) '~variant-name)))
       (defn ~variant-name [~@variant-args]
         {:pre [~@(map list variant-field-predicates variant-field-names)]}
         {:type '~type-name
          :variant '~variant-name
          ;;使用array-map是用来保证顺序
          :values (array-map
                    ~@(mapcat #(list (keyword %1) %1) variant-field-names))}))))

(defmacro define-datatype [type-name type-predicate-name & variants]
  `(do
     (data-type-predicate ~type-name ~type-predicate-name)
     ~@(map (fn [v] `(data-type-variant ~v ~type-name)) variants)))

(defmacro cases [type-name expression & clauses]
  (let [variant (gensym)]
    `(let [~variant ~expression]
       ;;因为在data-type-variant这个宏中已经为每一个variant-name生成了一个类似构造函数的东西
       (if (not= (:type ~variant) '~type-name)
         (throw (Exception. (str "invalid type expected " '~type-name " found '" (:type ~variant) "'"))))
       (cond ~@(mapcat (fn [clause]
                         (let [variant-name (car clause)]
                           (if (= variant-name 'else)
                             `(:else ~@(cdr clause))
                             (let [[_ field-names & consequent] clause]
                               `((= (:variant ~variant) '~variant-name)
                                  (apply
                                    (fn [~@field-names]
                                      ~@consequent)
                                    ;;这些值是在定义表达式的时候就求值好了的
                                    (vals (:values ~variant))))))))
                       clauses)))))

(defn data-value [data field-name]
  (get (get data :values) field-name))