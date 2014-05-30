(ns hont.core-test
  (:require [clojure.test :refer :all]
            [hont.core :refer [hont]]))

(def votes
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


(deftest european2014
  (is (= results (hont votes seats))))

(run-tests)
