(ns rle.core)

(defn pack
  "Tail-recursive way to pack a sequence"
  [coll]
  (letfn [(pack* [[f & r :as S] acc]
            (when (seq S)
              (let [[packed tail] (split-with #{f} S)]
                (if (seq tail)
                  (recur tail (conj acc packed))
                  (conj acc packed)))))]
    (pack* coll [])))

(defn mundane-pack
  "Mundane recursive way to pack a sequence"
  [[f & r :as S]]
  (if (seq S)
    (let [[packed tail] (split-with #{f} S)]
      (if (seq tail)
        (cons packed (pack tail))
        [packed]))
    [nil]))

(defn rle
  "Run-length encode a sequence"
  [coll]
  (map #(vector (count %) (first %))
       (pack coll)))

(defn rle [s] (map (juxt count first) (partition-by identity s)))

(defn rld [s] (mapcat (fn [[n e]] (repeat n e)) s))

(def S ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e])

(pack S)
;=> [(a a a a) (b) (c c) (a a) (d) (e e e e)]

(rle S)
;=> ([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])

