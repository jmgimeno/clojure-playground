(ns macros-kata.part2)

; Code from post: How to Make an SQL From Clojure or My Macros Kata: Part 2
; by Alexander Turok

; http://blog.alex-turok.com/2014/06/how-to-make-sql-from-clojure-or-my.html

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

;PART2 starts here

;code 1

(def table [
             {:a 1 :b 100 :c "100" :d 4}
             {:a 2 :b 200 :c "200" :d 3}
             {:a 3 :b 300 :c "300" :d 2}
             {:a 4 :b 400 :c "400" :d 1}])

(select :c from table)
;=> ({:c "100"} {:c "200"} {:c "300"} {:c "400"})

;code 2

(select :a :b from table where :a < :d)
;=> ({:a 1, :b 100} {:a 2, :b 200})


(select :a :b from table where :a = 2 or (:d < 3 and :b > 300))
;=> ({:a 2, :b 200} {:a 4, :b 400})

;code 3

(defmacro select [& what]
  (let [fields (set
                 (take-while #(not= 'from %) what))
        source (fnext
                 (drop-while #(not= 'from %) what))
        conditions (next (drop-while #(not= 'where %) what))]
    `(map (fn [record#]
            (into {} (filter #(~fields (first %)) record#)))
          (filter #(condition % ~@conditions) ~source))))

;code 4

(defmacro select [& what]
  (let [fields (set
                 (take-while #(not= 'from %) what))
        source (fnext
                 (drop-while #(not= 'from %) what))
        conditions (take-while #(not= 'orderby %) (next (drop-while #(not= 'where %) what)))
        orderings (next (drop-while #(not= 'orderby %) what))]
    `(map (fn [record#]
            (into {} (filter #(~fields (first %)) record#)))
          (filter #(condition % ~@conditions) ~source))))

;code 5
(defmacro order [orderings what]
  (if (empty? orderings)
    what
    nil))

(defmacro select [& what]
  (let [fields (set
                 (take-while #(not= 'from %) what))
        source (fnext
                 (drop-while #(not= 'from %) what))
        conditions (take-while #(not= 'orderby %) (next (drop-while #(not= 'where %) what)))
        orderings (next (drop-while #(not= 'orderby %) what))]
    `(order ~orderings
            (map (fn [record#]
                   (into {} (filter #(~fields (first %)) record#)))
                 (filter #(condition % ~@conditions) ~source)))))

;code 6
(select :c from table where :a > 1)
;=> ({:c "200"} {:c "300"} {:c "400"})

(select :b :c from table where :a > 1 orderby :b)
;=> nil

;code 7

(defmacro order [orderings what]
  (if (empty? orderings)
    what
    `(sort-by (juxt ~@orderings) ~what)))

(def table [
             {:a 1 :b 400 :c "200" :d 4}
             {:a 2 :b 300 :c "100" :d 3}
             {:a 3 :b 200 :c "400" :d 2}
             {:a 4 :b 100 :c "300" :d 1}])

(select :b :c from table where :a > 1 orderby :b)
;=> ({:c "300", :b 100} {:c "400", :b 200} {:c "100", :b 300})

(pprint (select :a :b :c :d from table orderby :d))
;=> ({:a 4, :c "300", :b 100, :d 1}
;=> {:a 3, :c "400", :b 200, :d 2}
;=> {:a 2, :c "100", :b 300, :d 3}
;=> {:a 1, :c "200", :b 400, :d 4})

;code 8

(defmacro order [orderings what]
  (if (empty? orderings)
    what
    (let [orderings-desym
          (vec (map (fn [s]
                      (cond (= 'desc s) -1
                            (= 'asc s) 1
                            (keyword? s) s))))]
      `(sort-by (juxt ~@orderings) ~what))))

;code 9

(defmacro order [orderings what]
  (if (empty? orderings)
    what
    (let [orderings-desym
          (vec (map (fn [s]
                      (cond (= 'desc s) -1
                            (= 'asc s) 1
                            (keyword? s) s))))
          orderings-separated
          (->> orderings-desym
               (partition-by keyword?)
               (map (partial interpose 1))
               flatten
               (partition-all 2))]
      `(sort-by (juxt ~@orderings) ~what))))

;code 10

;0. [:a :b :c 1 :d :e -1 :f] ;initial orderings
;1. ((:a :b :c) (1) (:d :e) (-1) (:f)) ;partition-by keyword?
;2. ((:a 1 :b 1 :c) (1) (:d 1 :e) (-1) (:f)) ;map (partial interpose 1)
;3. (:a 1 :b 1 :c 1 :d 1 :e -1 :f) ;flatten
;4. ((:a 1) (:b 1) (:c 1) (:d 1) (:e -1) (:f)) ;partition-all 2

;code 11

(defn order-fn
  ([k]
   k)
  ([k ord]
   (if (neg? ord)
     `(fn [r#] (- (~k r#)))
     k)))

(defmacro order [orderings what]
  (if (empty? orderings)
    what
    (let [orderings-desym
          (map (fn [s]
                 (cond (= 'desc s) -1
                       (= 'asc s) 1
                       (keyword? s) s)) orderings)
          orderings-separated
          (->> orderings-desym
               (partition-by keyword?)
               (map (partial interpose 1))
               flatten
               (partition-all 2))
          order-funcs
          (map #(apply order-fn %) orderings-separated)]
      `(sort-by (juxt ~@order-funcs) ~what))))

(pprint (select :a :b :c :d from table orderby :a desc))

;=> ({:a 4, :c "300", :b 100, :d 1}
;=> {:a 3, :c "400", :b 200, :d 2}
;=> {:a 2, :c "100", :b 300, :d 3}
;=> {:a 1, :c "200", :b 400, :d 4})

