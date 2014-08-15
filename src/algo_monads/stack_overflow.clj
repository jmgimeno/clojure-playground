(ns algo-monads.stack-overflow
  (:require [clojure.algo.monads :as m]))

; http://dev.clojure.org/jira/browse/ALGOM-5

(m/defmonadfn fold-m [f a xs]
              (reduce (fn [macc x] (m-bind macc #(f % x))) (m-result a) xs))

(defn add [s]
  (m/run-cont
    (m/with-monad m/cont-m
                (m/call-cc
                  (fn [k]
                    (fold-m (fn [a n]
                              (if (== n 8888) (k 1)
                                              (m-result (+ a n))))
                            0 s))))))

;;(add (range 1 10000)) ;; stack overflow

(defn m-result-cont [v] (fn [c] [c v ::cont]))

(defn m-bind-cont [mv f] (fn [c] [mv (fn [v] [(f v) c ::cont ]) ::cont]))

(m/defmonad cont-tramp-m
          [m-result m-result-cont
           m-bind  m-bind-cont])

(defn call-cc-tramp [f]
  (fn [c] [(f (fn [v] (fn [_] [c v ::cont]))) c ::cont]))

(defn run-cont-tramp [cont]
  (loop [cv (cont identity)]
    (if (and (vector? cv)
             (= ::cont (nth cv 2)))
      (recur ((first cv) (second cv)))
      cv)))

(defn add2 [s]
  (run-cont-tramp
    (m/with-monad m/cont-m
                (call-cc-tramp
                  (fn [k]
                    (fold-m (fn [a n]
                              (if (= n 8888)
                                (k 1)
                                (m-result (+ a n))))
                            0 s))))))

;; Does not work as expected (raises StackOverflow)

;; (add2 (range 1 1000000)) ;; -> 1

