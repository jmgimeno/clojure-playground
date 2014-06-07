(ns logic-tutorials.episode1
  (:refer-clojure :exclude [==]))

; First tutorial on Logic Programming by Tim Baldridge

(defn lvar
  ([]   (lvar ""))
  ([nm] (gensym (str nm "_"))))

(defn lvar? [v]
  (symbol? v))

(defn walk [s u]
  (if-let [pr (get s u)]
    (if (lvar? pr)
      (recur s pr)
      pr)
    u))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond (and (lvar? u)
               (lvar? v)
               (= u v)) s
          (lvar? u) (assoc s u v)
          (lvar? v) (assoc s v u)
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

(comment

  (walk {} (lvar "s"))

  (let [s (lvar "s")
        v (lvar "v")]
    (walk {s v v 42} s))

  (unify {} (lvar "s") 42)

  (unify {} (lvar "s") (lvar "v"))

  (unify {} 1 2)

  (unify {} 1 1)

  ((== 1 2) {})

  ((== 1 1) {})

  ((== (lvar "foo") 1) {})

  (let [a (lvar "a")
        b (lvar "b")]
    ((-disj (-conj
              (== a 42)
              (== b a))
            (== b 11)) {}))

  )
