(ns ants.core-test
  (:require [clojure.test :refer :all]
            [ants.core :refer :all]))

(deftest ant-moves-leftRemoved stupid line
  (testing
    (is (= 1 2))))

(run-tests)

