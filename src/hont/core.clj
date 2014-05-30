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

(defn next-state [[assigned quotients]]
  (let [[party _] (find-max quotients)
        quotients (update-in quotients [party] rest)]
    [(conj assigned party) quotients]))

(defn into-state [quotients]
  (next-state [[] quotients]))

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

(comment

  (set! *print-length* 10)

  (def european2014
    {:pp   4074363
     :psoe 3596324
     :lip  1562597
     :pod  1245948
     :upyd 1015994
     :ceu   850690
     :epdd  629071
     :cs    495114
     :lpd   324534
     :pe    299884})

  (def seats 54)

  (def results
    {:pp   16
     :psoe 14
     :lip   6
     :pod   5
     :upyd  4
     :ceu   3
     :epdd  2
     :cs    2
     :lpd   1
     :pe    1})

  (= results (hont european2014 seats))

  )

