(ns transients-tutorials.episode1
  (:import (clojure core$alength)
           (sun.jvm.hotspot.ci ciBaseObject)))

(defprotocol IArrayColl
  (-nth [this idx])
  (-append [this val])
  (-append! [this val]))

(deftype ArrayColl [^:volatile-mutable cnt ^objects arr]
  IArrayColl
  (-nth [this idx]
    (assert (< idx cnt))
    (aget arr idx))
  (-append [this val]
    (if (< cnt (alength arr))
      (let [new-array (aclone arr)]
        (aset new-array cnt val)
        (ArrayColl. (inc cnt) new-array))
      (let [new-array (object-array (* 2 (alength arr)))]
        (System/arraycopy arr 0 new-array 0 cnt)
        (aset new-array cnt val)
        (ArrayColl. (inc cnt) new-array))))
  (-append! [this val]
    (if (< cnt (alength arr))
      (do (aset arr cnt val)
          (set! cnt (inc cnt))
          this)
      (let [new-array (object-array (* 2 (alength arr)))]
        (System/arraycopy arr 0 new-array 0 cnt)
        (aset new-array cnt val)
        (ArrayColl. (inc cnt) new-array)))))

(defn array-coll []
  (ArrayColl. 0 (object-array 1)))

(comment
  (array-coll)

  (-> (array-coll)
      (-append 1)
      (-append 2)
      (-append 3))

  (-> (array-coll)
      (-append! 1)
      (-append! 2)
      (-append! 3))

  (-> (array-coll)
      (-append! 1)
      (-append! 2)
      (-append! 3)
      (-nth 2))

  (let [a (array-coll)
        b (-> a (-append! 1))]
    [a b])

  (let [a (array-coll)
        b (-> a (-append! 1))]
    (identical? a b))

  (let [a (array-coll)
        b (-> a (-append! 1) (-append! 2))]
    (identical? a b))

  (let [a (array-coll)]
    (dotimes [x 100]
      (-append! a x))
    (-nth a 42))

  )

