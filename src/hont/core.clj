(ns hont.core
  (:require [clojure.pprint :refer [pprint]]))

(defn votes-seq [votes]
  (map / (repeat votes) (iterate inc 1)))

(defn quotients [results]
  (zipmap (keys results)
          (map votes-seq (vals results))))

(defn first-quotient [[_ [fs & _]]] fs)

(defn find-max [quotients]
  (reduce (partial max-key first-quotient)
          quotients))

(defn into-state [quotients]
  [[] quotients])

(defn next-state [[assigned quotients]]
  (let [[party biggest] (find-max quotients)]
    [(conj assigned party) (assoc quotients party (rest biggest))]))

(defn not-enough [seats]
  (fn [[assigned _]]
    (< (count assigned) seats)))

(defn hont-seq [quotients]
  (iterate next-state (into-state quotients)))

(defn hont [results seats]
  (->> results
       quotients
       hont-seq
       (drop-while (not-enough seats))
       ffirst
       frequencies))


