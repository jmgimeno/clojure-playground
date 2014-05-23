(ns turing-machine.core-test
  (:require [clojure.test :refer :all]
            [turing-machine.core :refer :all])
  (:refer-clojure :exclude [read write]))

(deftest moving-inside
  (testing "I can move inside a tape"
    (let [tape (make-tape [1 2 3 4])]
      (is (= 1 (read tape))))))

(deftest replacing
  (testing "I can replace the current symbol"
    (let [tape (make-tape [1 2 3 4])]
      (is (= 10 (read (write tape 10)))))))

(run-tests)