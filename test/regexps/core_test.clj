(ns regexps.core-test
  (:require [clojure.test :refer :all]
            [regexps.core :refer :all]
            [clojure.java.io :as jio]))

(def regexps [#"^a.*" #"^b.*"])

(deftest match-lines-test
  (testing "First two non blank lines match"
    (is (match-lines "resources/lines1.txt" regexps)))
  (testing "First non blank does not match"
    (is (not (match-lines "resources/lines2.txt" regexps))))
  (testing "All lines match but less lines than regexps"
    (is (not (match-lines "resources/lines3.txt" regexps)))))




