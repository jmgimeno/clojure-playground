(ns algo-monads.nonblocking
  (:require [clojure.algo.monads :as m]))

; nonblocking list comprehension with cps monad
; http://www.dustingetz.com/2012/07/11/cps-monad.html

; Plumbing

(m/with-monad m/cont-m
              (def mk-cont m-result))

(defn mk-cps
  "wrap a function `f` taking only a callback, suitable for use inside (maybe-t cont-m)
   we use maybe-monad to bail out of the cps monad comprehension without blocking, and
   when we have the async response we 'jump back into' the monad comprehension"
  [f]
  (m/call-cc
    (fn [c] ;`c` is a "callback" to the business logic in the monad comprehension
      (let [callback (fn [result]
                       (m/run-cont (c result)))]
        (f callback)
        (mk-cont nil)))))

; Delayed

(defn delayed-async [retval callback]
  (future (Thread/sleep 1000)
          (callback retval)))

(def async-get-fav (partial delayed-async :track1))
(def async-get-favr (partial delayed-async :user1))

(defn recs-4 [callback]
  (m/domonad (m/maybe-t m/cont-m)
             [fav (mk-cps async-get-fav)
              favr (mk-cps async-get-favr)]
             (callback fav favr)))

;; (m/run-cont (recs-4 prn))

(def async-get-favs (partial delayed-async [:track1 :track2 :track3]))
(def async-get-favrs (partial delayed-async [:user1 :user2 :user3]))

(defn recs-5 [callback]
  (m/domonad (m/sequence-t (m/maybe-t m/cont-m))
             [fav (mk-cps async-get-favs)
              favr (mk-cps async-get-favrs)]
             (callback fav favr)))

;; (m/run-cont (recs-5 prn))