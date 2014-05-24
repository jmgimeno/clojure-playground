(ns wheels.core)

;; Calculate wheels for clojure.contrib.lazy-seqs.primes

(defn cycle-seq [n xs]
  (for [_ (range n) x xs] x))

(defn rotate-left [seq]
    (concat (drop 1 seq) (take 1 seq)))

(defn wheel [n]
  (if (zero? n)
    [[2] [1]] ;; Candidates are 2..
    (let [[primes steps]  (wheel (dec n))
	  last-prime      (last primes)
	  new-prime       (+ last-prime (first steps))
	  valid?          #(pos? (mod % last-prime))
	  candidates      (->> steps
			       rotate-left
			       (cycle-seq last-prime)
			       (reductions + new-prime)
			       (filter valid?))
	  new-steps       (map - (rest candidates) candidates)]
      [(conj primes new-prime) new-steps])))

;; (defn filter-and-merge
;;   ([p1 p2 cs]
;;      (filter-and-merge p1 p2 cs 0))
;;   ([p1 p2 cs acc] 
;;      (when-let [[c & cs] cs]
;;        (if (zero? (mod (+ p2 c) p1))
;; 	 (filter-and-merge p1 (+ p2 c) cs (+ c acc))
;; 	 (cons (+ c acc)
;; 	       (filter-and-merge p1 (+ p2 c) cs 0))))))

;; (defn filter-and-merge2
;;   ([last-prime candidate steps]
;;      (filter-and-merge2 last-prime candidate steps 0))
;;   ([last-prime candidate steps accum-step]
;;      (when-let [[step & steps] steps]
;;        (let [multiple? #(zero? (mod % last-prime))
;; 	     new-candidate (+ candidate step)]
;; 	 (if (multiple? new-candidate)
;; 	   (filter-and-merge2 last-prime new-candidate steps (+ step accum-step))
;; 	   (cons (+ step accum-step)
;; 		 (filter-and-merge2 last-prime new-candidate steps 0)))))))

;; (defn filter-and-merge3 [last-prime new-prime steps]
;;   (let [valid? #(pos? (mod % last-prime))
;; 	candidates (reductions + new-prime steps)
;; 	valid-candidates (filter valid? candidates)]
;;     (map - (rest valid-candidates) valid-candidates)))

;; (defn wheel-2 [n]
;;   (if (zero? n)
;;     [[2] [1]] ;; Candidates are 2..
;;     (let [[primes steps]  (wheel-2 (dec n))
;; 	  last-prime      (last primes)
;; 	  new-prime       (+ last-prime (first steps))
;; 	  candidates      (cycle-seq last-prime (rotate-left steps))
;; 	  new-steps       (filter-and-merge3 last-prime new-prime candidates)]
;;       [(conj primes new-prime) new-steps])))

