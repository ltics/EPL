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

(def lambda fn)