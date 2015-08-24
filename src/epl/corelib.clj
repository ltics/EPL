(ns epl.corelib
  (:require [epl.cota :refer :all]))

(defmacro data-type-predicate [type-name type-predicate-name]
  `(defn ~type-predicate-name [~'variant]
     (= (:type ~'variant) '~type-name)))

(defmacro data-type-variant [variant type-name]
  (let [variant-name (first variant)
        variant-spec (rest variant)
        variant-field-names (map first variant-spec)
        variant-args (mapcat #(reverse (take (if (= (second %1) '&) 2 1) %1)) variant-spec)
        variant-field-predicates (map last variant-spec)]
    (prn variant-name)
    (prn variant-spec)
    (prn variant-field-names)
    (prn variant-args)
    (prn variant-field-predicates)
    `(do
       (defn ~(symbol (str variant-name "?")) [data#]
         (and (= (get data# :type) '~type-name)
              (= (get data# :variant) '~variant-name)))
       (defn ~variant-name [~@variant-args]
         {:pre [~@(map list variant-field-predicates variant-field-names)]}
         {:type '~type-name
          :variant '~variant-name
          :values (array-map
                    ~@(mapcat #(list (keyword %1) %1) variant-field-names))}))))

(defmacro define-datatype [type-name type-predicate-name & variants]
  `(do
     (data-type-predicate ~type-name ~type-predicate-name)
     ~@(map (fn [v] `(data-type-variant ~v ~type-name)) variants)))

(defmacro cases [type-name expression & clauses]
  (let [variant (gensym)]
    `(let [~variant ~expression]
       (if (not (= (:type ~variant) '~type-name))
         (throw (Exception. (str "invalid type expected " '~type-name " found '" (:type ~variant) "'"))))
       (cond ~@(mapcat (fn [clause]
                         (let [variant-name (first clause)]
                           (if (= variant-name 'else)
                             `(:else ~@(rest clause))
                             (let [[_ field-names & consequent] clause]
                               `((= (:variant ~variant) '~variant-name)
                                  (apply
                                    (fn [~@field-names]
                                      ~@consequent)
                                    (vals (:values ~variant))))))))
                       clauses)))))

(defn data-value [data field-name]
  (get (get data :values) field-name))

(define-datatype env env?
  (empty-env)
  (non-empty-env
    (var symbol?)
    (val (fn [x] true?))
    (saved-env env?)))

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
                  (and (not (= search-var bound-var))
                       (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or (occurs-free? search-var rator)
                   (occurs-free? search-var rand))))))

(occurs-free? 'x (var-exp 'x))
(occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))