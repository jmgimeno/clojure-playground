(ns lazy.core)

(defn traced-seq [seq]
  (map #(do (println (str "[" % "]")) %) seq))

(def ones (lazy-seq (cons 1 ones))) ; (repeat 1)

(def nats (lazy-seq (cons 1 (map inc nats)))) ; (iterate inc 1)

(def nats-2 (lazy-seq (cons 1 (map + (repeat 1) nats-2))))

(def fibs (lazy-seq (list* 0 1 (map + fibs (drop 1 fibs)))))

(def facts (lazy-seq (cons 1 (map * facts (iterate inc 1)))))

(defn sieve [seq]
  (lazy-seq
   (let [prime (first seq)]
     (cons
      prime
      (sieve (remove #(zero? (mod % prime)) (rest seq)))))))

(def primes (sieve (iterate inc 2)))

(defn drop-nths [n seq]
  (lazy-seq
   (concat
    (take (- n 1) seq)
    (drop-nths n (drop n seq)))))

(defn sieve-2 [nums]
  (lazy-seq
   (let [prime (first nums)]
     (cons
      prime
      (sieve-2 (drop-nths prime (rest nums)))))))

(def primes-2 (sieve-2 (iterate inc 2)))

;; From comp.lang.clojure google group

(def primes-3
     (concat 
      [2]
      (lazy-seq 
       (let [primes-from 
	     (fn primes-from 
	       [n] 
	       (if (some #(zero? (rem n %)) 
			 (take-while #(<= (* % %) n) primes-3)) 
		 (recur (+ n 2)) 
		 (lazy-seq (cons n (primes-from (+ n 2))))))] 
	 (primes-from 3)))))

;; Functional pearl: Enumerating Rationals

(defn- diags 
  ([yss]           
     (diags [] yss))
  ([xss [ys & yss]] 
     (lazy-seq
      (concat (map first xss)
	      (diags (cons ys (map rest xss)) yss)))))
(def rats
     (diags 
      (for [n (iterate inc 1)] 
	      (for [m (iterate inc 1)] 
		(/ m n)))))

(def rats-2
     (for [d (iterate inc 1)
	   m (range 1 d)]
       (/ m (- d m))))

;; Lazy quick-sort (The Joy of Clojure, Listing 6.3)

(defn- cons-when [v coll]
  (if (empty? v) coll (cons v coll)))

(defn- sort-parts [work]
  "Lazy, tail-recursive, incremental quicksort. Works against
   and creates partitions based on the pivot, defined as work"
  (lazy-seq
   (loop [[part & parts :as work] work]
     (when work
       (if (coll? part)
	 (let [[pivot & xs] part
	       smaller? #(< % pivot)]
	   (recur (cons-when 
		   (filter smaller? xs)
		   (cons pivot
			 (cons-when
			  (remove smaller? xs)
			  parts)))))
	 (cons part (sort-parts parts)))))))

(defn qsort [xs]
     (sort-parts (list xs)))
