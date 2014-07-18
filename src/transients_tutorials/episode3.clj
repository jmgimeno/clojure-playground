(ns transients-tutorials.episode3
  (:require [clojure.repl :refer [source]]))

(comment

  (transient [])

  (conj (transient []) 1)

  (conj! (transient []) 1)

  (let [start (transient {})]
    (loop [last start
           current start
           idx 0]
         (when (< idx 42)
           (println (identical? last current) idx)
           (recur current (assoc! current idx idx) (inc idx)))))

  (seq (persistent! (transient [])))

  (persistent! (reduce
                 (fn [acc x]
                   (conj! acc x))
                 (transient [])
                 (range 10)))

  (source into)

  (dotimes [x 100]
    (time (do (into [] (range 1000000))
              nil)))

  (defn into-slow [orig s]
    (reduce conj orig s))

  (dotimes [x 100]
    (time (do (into-slow [] (range 1000000))
              nil)))

  (let [v (conj! (transient []) 42)]
    @(future (identical? v (conj! v 22))))

  (let [v (persistent! (conj! (transient []) 42))]
    @(future (let [v (transient v)]
               (identical? v (conj! v 22)))))

  (let [v (into [] (range 1000000))]
    (time (dotimes [x 1000]
            (persistent! (transient v)))))

  ; 1) transients are opaque
  ; 2) use them as if they were persistent
  ; 3) constant time(-ish) to and from
  ; 4) don't give them to other threads







  )
