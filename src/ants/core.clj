(ns ants.core)

;; Positions

(defn make-positions [num-ants gap]
  (take num-ants (iterate (partial + gap) 0)))

(def positions (make-positions 5 4))

(def leftmost-pos (apply min positions))
(def rightmost-pos (apply max positions))

;; Movement

(defn rand-direction []
  (rand-nth [:left :right]))

(def swap-direction
  {:left  :right
   :right :left})

(defn move-left [delta]
  (fn [pos]
    (- pos delta)))

(defn move-right [delta]
  (fn [pos]
    (+ pos delta)))

(def move-fn
  {:left move-left
   :right move-right})

(defn move-ant [delta {dir :direction :as ant}]
  (update-in ant [:position] ((move-fn dir) delta)))

(defn move-ants [delta ants]
  (map (partial move-ant delta) ants))

;; Ants

(defn make-ant [pos dir]
  {:position  pos
   :direction dir})

(defn make-ants []
  (map make-ant
       positions
       (repeatedly rand-direction)))

;; Collision

(defn will-collide [[{dir1 :direction} {dir2 :direction}]]
  (and (= dir1 :right) (= dir2 :left)))

(defn all-collisions [ants]
  (filter will-collide (partition 2 1 ants)))

(defn time-to-collision [[{pos1 :position} {pos2 :position}]]
  (/ (- pos2 pos1) 2))

(defn time-to-next-collision [ants]
  (let [collisions (all-collisions ants)]
    (if (seq? collisions)
      (apply min (map time-to-collision collisions))
      Integer/MAX_VALUE)))

(defn collides-left [{pos-c :position dir-c :direction}
                     {pos-l :position dir-r :direction}]
  (and (= pos-c pos-l)
       (= dir-c :left)
       (= dir-r :right)))

(defn collides-right [{pos-c :position dir-c :direction}
                      {pos-r :position dir-r :direction}]
  (and (= pos-c pos-r)
       (= dir-c :right)
       (= dir-r :left)))

(defn collides [ant-l ant-c ant-r]
  (or (collides-left ant-c ant-l)
      (collides-right ant-c ant-r)))

;; Fall

(defn time-to-fall-left [{pos :position dir :direction}]
  (if (= dir :left)
    (- pos leftmost-pos)
    Integer/MAX_VALUE))

(defn time-to-fall-right [{pos :position dir :direction}]
  (if (= dir :right)
    (- rightmost-pos pos)
    Integer/MAX_VALUE))

(defn time-to-next-fall [ants]
  (let [leftmost (first ants)
        rightmost (last ants)]
    (min (time-to-fall-left leftmost)
         (time-to-fall-right rightmost))))

(defn falls-left [{pos :position dir :direction}]
  (and (<= pos leftmost-pos) (= dir :left)))

(defn falls-right [{pos :position dir :direction}]
  (and (<= rightmost-pos pos) (= dir :right)))

(defn falls [ant]
  (or (falls-left ant)
      (falls-right ant)))

;; Event

(defn time-to-next-event [ants]
  (min (time-to-next-collision ants)
       (time-to-next-fall ants)))

;; Step

(defn update-ant [[ant-l ant ant-r]]
  (cond
   (falls ant)                []
   (collides ant-l ant ant-r) [(update-in ant [:direction] swap-direction)]
   :else                      [ant]))

(defn update-ants [ants]
  (let [dummy-left (make-ant leftmost-pos :left)
        dummy-right (make-ant rightmost-pos :right)
        extended-ants (concat [dummy-left] ants [dummy-right])]
    (mapcat update-ant (partition 3 1 extended-ants))))

(defn step [ants]
  (let [delta (time-to-next-event ants)
        moved (move-ants delta ants)]
    (update-ants moved)))
