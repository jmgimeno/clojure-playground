(ns macros-kata.part1)

; Code from post: How to Make an SQL from Clojure or My Macros Kata
; by Alexander Turok

; http://blog.alex-turok.com/2014/05/how-to-make-sql-from-clojure-or-my.html

; Similar (but not equal) to: https://github.com/aturok/cql

; code 1

(def table [
             {:a 1 :b 100 :c "100" :d 4}
             {:a 2 :b 200 :c "200" :d 3}
             {:a 3 :b 300 :c "300" :d 2}
             {:a 4 :b 400 :c "400" :d 1}])

(select :a :b from table)

; code 2

(defmacro select [& what]
  (let [fields (set
                 (take-while #(not= 'from %) what))
        source (fnext
                 (drop-while #(not= 'from %) what))]
    `(map (fn [record#]
            (into {} (filter #(~fields (first %)) record#)))
          ~source)))

; code 3
(select :a :b from table)

;=> ({:a 1, :b 100} {:a 2, :b 200} {:a 3, :b 300} {:a 4, :b 400})

; code 4

(defmacro condition [t k1 op k2]
  `(~op (~k1 ~t) (~k2 ~t)))

; code 5

(macroexpand '(condition table :a < :d))
;=> (< (:a table) (:d table))

(macroexpand '(condition table :b = :c))
;=> (= (:b table) (:c table))

; code 6

(defmacro condition [t k1 op k2]
  (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
        v2 (if (keyword? k2) `(~k2 ~t) k2)]
    `(~op ~v1 ~v2)))

; code 7

(macroexpand '(condition table :d not= :a))
;=> (not= (:d table) (:a table))
; no checks for keyword, table accessed in both cases

(macroexpand '(condition table :a < 3))
;=> (< (:a table) 3)
; no checks for keyword, and table accessed only once
; while for 3 we have just 3


; code 8

(defmacro select [& what]
  (let [fields (set
                 (take-while #(not= 'from %) what))
        source (fnext
                 (drop-while #(not= 'from %) what))
        conditions (next (drop-while #(not= 'where %) what))]
    `(map (fn [record#]
            (into {} (filter #(~fields (first %)) record#)))
          (filter #(condition % ~@conditions) ~source))))

; code 9

(select :a :b from table where :a < :d)
;=> ({:a 1, :b 100} {:a 2, :b 200})
(select :d :c from table where :d >= 3)
;=> ({:c "100", :d 4} {:c "200", :d 3})
(select :a :b :c :d from table where :a = :b)
;=> ()

; code 10

(defmacro condition
  ([t k1 op k2]
   (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
         v2 (if (keyword? k2) `(~k2 ~t) k2)]
     `(~op ~v1 ~v2)))
  ([t k1 op1 k2 link & other]
   `(~link
     (condition ~t ~k1 ~op1 ~k2)
     (condition ~t ~@other))))

; code 11

(select :a :b from table where :a <= 2 or :c = “400”)
;=> ({:a 1, :b 100} {:a 2, :b 200} {:a 4, :b 400})

(select :a :b from table where :a = 1 or :a = 2 or :a = 4 or :a = 5 or :a = 6 or :a = 7)
;=> ({:a 1, :b 100} {:a 2, :b 200} {:a 4, :b 400})

; code 12

(defmacro condition
  ([t complex]
   `(condition ~t ~@complex))
  ([t k1 op k2]
   (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
         v2 (if (keyword? k2) `(~k2 ~t) k2)]
     `(~op ~v1 ~v2)))
  ([t k1 op1 k2 link & other]
   `(~link
     (condition ~t ~k1 ~op1 ~k2)
     (condition ~t ~@other))))

; code 13

(select :a :b from table where :a = 2 or (:d < 3 and :b > 300))
;=> ({:a 2, :b 200} {:a 4, :b 400})

(select :a :b from table where (:a <= 2 or :c = “400”))
;=> ({:a 1, :b 100} {:a 2, :b 200} {:a 4, :b 400})

; code 14

(defmacro condition
  ([t]
   `true)
  ([t complex]
   `(condition ~t ~@complex))
  ([t k1 op k2]
   (if (seq? k1)
     `(~op
       (condition ~t ~k1)
       (condition ~t ~k2))
     (let [v1 (if (keyword? k1) `(~k1 ~t) k1)
           v2 (if (keyword? k2) `(~k2 ~t) k2)]
       `(~op ~v1 ~v2))))
  ([t k1 op1 k2 link & other]
   (if (seq? k1)
     `(~op1
       (condition ~t ~k1)
       (condition ~t ~k2 ~link ~@other))
     `(~link
       (condition ~t ~k1 ~op1 ~k2)
       (condition ~t ~@other)))))

; code 15

(select :a :b from table where (:d < 3 and :b > 300) or :a = 2)
;=> ({:a 2, :b 200} {:a 4, :b 400})

(select :c from table)
;=> ({:c "100"} {:c "200"} {:c "300"} {:c "400"})
