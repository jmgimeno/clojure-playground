(ns logic-tutorials.episode4
  (:refer-clojure :exclude [==]))

(defn lvar
  ([] (lvar ""))
  ([nm] (gensym (str nm "_"))))

(defn lvar? [v]
  (symbol? v))

(defn walk [s u]
  (let [pr (get s u ::not-found)]
    (if-not (identical? pr ::not-found)
      (if (lvar? pr)
        (recur s pr)
        pr)
      u)))

(defprotocol ILCons
  (lfirst [this])
  (lnext [this]))

(defn lcons? [x]
  (satisfies? ILCons x))

(extend-type clojure.lang.ISeq
  ILCons
  (lfirst [this]
    (first this))
  (lnext [this]
    (next this)))

(defrecord LCons [h t]
  ILCons
  (lfirst [this]
    h)
  (lnext [this]
    t))

(defn lcons [h t]
  (->LCons h t))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond (and (lvar? u) (lvar? v) (= u v)) s
          (lvar? u) (assoc s u v)
          (lvar? v) (assoc s v u)
          (and (lcons? u) (lcons? v)) (let [s (unify s (lfirst u) (lfirst v))]
                                        (and s (recur s (lnext u) (lnext v))))
          :else (and (= u v) s))))

(defn == [a b]
  (fn [s]
    (if-let [v (unify s a b)]
      [v]
      [])))

(defn -conj
  ([a] a)
  ([a b]
   (fn [s]
     (for [aret (a s)
           :when aret
           bret (b aret)
           :when bret]
       bret)))
  ([a b & more]
   (-conj a (apply -conj b more))))

(defn -disj [& goals]
  (fn [s]
    (mapcat (fn [goal] (goal s)) goals)))

(defn conde [& goals]
  (apply -disj (map (partial apply -conj) goals)))

(defmacro fresh [lvars & goals]
  `(let [~@(vec (mapcat (fn [var]
                          `[~var (lvar ~(name var))])
                        lvars))]
     ~(if (> (count goals) 1)
        `(-conj ~@goals)
        (first goals))))

(defn reify-vars [s lvars]
  (map (fn [s']
         (map (fn [lvar]
                (walk s' lvar)) lvars)) s))

(defmacro run [lvars & goals]
  `(let [lvars# ~(vec (map (fn [var]
                             `(lvar ~(name var)))
                           lvars))
         ~lvars lvars#
         r# (-conj ~@goals)]
     (reify-vars (r# {}) lvars#)))

(defn conso [h t o]
  (== (lcons h t) o))

(defn firsto [h t]
  (fresh [rest]
         (conso h rest t)))

(defn resto [t coll]
  (fresh [h]
         (conso h t coll)))

(defmacro defer [goal]
  `(fn [s#]
     (~goal s#)))

(defn membero [v coll]
  (conde
    [(firsto v coll)]
    [(fresh [t]
            (resto t coll)
            (defer (membero v t)))]))
(comment

  (run [q]
       (resto q (range 2)))

  (run [q]
       (membero q (range 10)))

  (take 10 (run [q]
                (membero q (range 100))))

  (run [q]
       (membero q (range 100))
       (membero q (range 10))
       (== q 1))

  ; We do depth-first-search

  (run [q]
       (conde
         [(conde
            [(== q 1)]
            [(== q 11)])]
         [(conde
            [(== q 2)]
            [(== q 22)])]
         [(== q 3)]))

  ; And we get 1 11 2 22 3

  ; But we want 1 2 3 11 22 (breath-first-search)

  )
