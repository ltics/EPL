(ns epl.cota)

(def car first)
(def cdr rest)
(def cadr #(car (cdr %)))
(def cddr #(cdr (cdr %)))
(def caadr #(car (car (cdr %))))
(def caddr #(car (cdr (cdr %))))
(def cdadr #(cdr (cadr %)))
(def cadddr #(car (cdr (cddr %))))

(def null? (fn [x]
             (if (seq? x)
               (empty? x)
               (nil? x))))

(defmacro lambda
  [& sigs]
  `(fn ~@sigs))

(def vector-ref nth)
(def vector-length count)

(defmacro define-datatype
  [type & body]
  (let [constructors (map (comp keyword car) body)
        fields (map (fn [inner-fields]
                      (vec (map str inner-fields))) (map cdr body))
        specs (zipmap constructors fields)]
    `(def ~type ~specs)))