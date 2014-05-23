(ns regexps.core
  (:require [clojure.java.io :as jio]))

(defn match-lines [from regexps]
  (let [lines (line-seq (jio/reader from))
        non-blank (remove empty? lines)
        pair-matches (map re-matches regexps non-blank)
        matches-until-fail (take-while identity pair-matches)]
    (= (count matches-until-fail) (count regexps))))

(defn match-lines2 [from regexps]
  (->> from
       jio/reader
       line-seq
       (remove empty?)
       (map re-matches regexps)
       (take-while identity)
       count
       (= (count regexps))))

(defn -main [& args]
  (let [regexps [#"^a.*" #"^b.*"]]
    (println (match-lines *in* regexps))))
