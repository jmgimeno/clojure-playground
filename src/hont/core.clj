(ns hont.core)

(defn votes-seq [votes]
  (map / (repeat votes) (iterate inc 1)))

(defn quotients [results]
  (into {}
	(map (fn [[party votes]] 
	       [party (votes-seq votes)])
	     results)))

(defn find-max [quotients]
  (reduce (fn [[_ [fs1 & _] :as fst]
	       [_ [fs2 & _] :as snd]]
	    (if (>= fs1 fs2) fst snd)) 
	  quotients))

(defn hont-seq [quotients]
  (lazy-seq
   (let [[party votes-seq] (find-max quotients)]
     (cons party (hont-seq (assoc quotients party (rest votes-seq)))))))

(defn hont [results seats]
  (let [quots (quotients results)
	hseq (hont-seq quots)]
    (frequencies
     (take seats hseq))))