(ns letona.core
  (:require [clojure.pprint :refer [pprint]]))

(def start 458)

(def end 14)

(defn init-state [n]
  {n [[]]})

(defn step [[n ps]]
  [{(* 2 n)     (map #(conj % :double) ps)}
   {(quot n 10) (map #(conj % :drop) ps)}])

(defn next-state [state]
  (apply merge-with concat (mapcat step state)))

(defn final-state? [state]
  (get state end))

(def final-state
  (first
    (drop-while
      (complement final-state?)
      (iterate next-state (init-state start)))))

(pprint (get final-state end))

