(ns core-async-tutorials.episode3
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >!]]
            [clojure.core.async.impl.protocols :as impl]))

;; error handling

(defmacro <!!? [c]
  `(let [v# (<!! ~c)]
     (if (instance? Throwable v#)
       (throw v#)
       v#)))

(defmacro go-try [& body]
  `(go (try ~@body
            (catch Throwable ex#
              ex#))))

(def log-c (async/chan 1024))

(defn to-proc< [in]
  (let [out (async/chan 1)]
    (async/pipe in out)
    out))

(defn pipe-ext [in out]
  (go (loop []
        (when-some [v (<! in)]
           (>! out v)
           (recur)))))

(defn map-ext [in f out]
  (go (loop []
        (when-some [val (<! in)]
           (let [val (try (f val)
                          (catch Throwable ex
                            (>!! log-c ["Failure: " ex f val])
                            :exception))]
             (cond
               (or (seq? val)
                   (vector? val)) (do (<! (async/onto-chan out val))
                                      (recur))

               (extends? impl/ReadPort (class val)) (do (<! (pipe-ext val out))
                                                        (recur))
               (identical? val :exception) (recur)
               (nil? val) (recur)
               :else (do (>! out val)
                         (recur))))))))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    (reduce
      (fn [prev-c [n f]]
        (let [out-c (async/chan n)]
          (dotimes [_ n]
            (map-ext prev-c f out-c))
          out-c))
      c
      p)))

(comment

  (go (assert false))

  (<!!? (go (assert false)))

  (try (<!!? (go-try (assert false)))
       (catch Throwable ex
         "got ex"))

  (try (<!!? (go-try 42))
       (catch Throwable ex
         "got ex"))


  (go (loop []
        (println "Got log item: " (<! log-c))
        (recur)))

  (let [c (async/chan 1024)]
    (>!! c 42)
    (>!! c 43)
    (<!! (pipeline< [4 inc
                     3 #(if (even? %)
                         (assert false (str "not odd" %))
                         %)
                     2 str]
                    c)))

  ; In mny execution first I get the exception log and then the value "43"

  (let [c (async/chan 1024)]
    (>!! c 42)
    (>!! c 43)
    (<!! (pipeline< [4 inc
                     2 range
                     3 #(if (even? %)
                         (assert false (str "not odd" %))
                         %)
                     2 str]
                    c)))


  (let [c (async/chan 1024)]
    (>!! c 42)
    (>!! c 43)
    (<!! (async/into [] (async/take 4 (pipeline< [4 inc
                                                  2 range
                                                  3 #(if (even? %)
                                                      (assert false (str "not odd" %))
                                                      %)
                                                  2 str]
                                                 c)))))
  )