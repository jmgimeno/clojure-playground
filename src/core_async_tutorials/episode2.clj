(ns core-async-tutorials.episode2
  (:require [clojure.core.async :as async :refer [<!! >!! go <! >!]]
            [clojure.core.async.impl.protocols :as impl]))

;; seq and chan pipelines

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
                   (let [val (f val)]
                     (cond
                       (or (seq? val)
                           (vector? val)) (do (<! (async/onto-chan out val))
                                              (recur))

                       (extends? impl/ReadPort (class val)) (do (<! (pipe-ext val out))
                                                                (recur))

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

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (pipeline< [4 inc
                     1 inc
                     2 dec
                     3 str]
                    c)))

  ;=> 43 OK

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (pipeline< [4 inc
                     1 inc
                     2 range
                     3 str]
                    c)))

  ;=> "0" OK


  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (pipeline< [4 inc
                     1 inc
                     2 (fn [x]
                         (async/to-chan (range x)))
                     3 str]
                    c)))

  ;=> "0" OK

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 4 (pipeline< [4 inc
                                                  1 inc
                                                  2 (fn [x]
                                                      (async/to-chan (range x)))
                                                  3 str]
                                                 c)))))
  ;=> ["0" "1" "2" "3"] NO!!!
  ;=> ["0" "3" "4" "5"] si 3 str
  ;=> ["0" "2" "3" "4"] si 2 str
  ;=> ["0" "1" "2" "3"] si 1 str


  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 10 (pipeline< [4 inc
                                                   1 inc
                                                   2 (fn [x]
                                                       (async/to-chan (range x)))
                                                   3 str]
                                                  c)))))

  ;=> ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"] NO!!!
  ;=> ["0" "3" "4" "5" "1" "2" "7" "6" "8" "9"] si 3 str
  ;=> ["0" "2" "3" "4" "1" "5" "6" "7" "8" "9"] si 2 str
  ;=> ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"] si 1 str

  (defn pause-rnd [x]
    (go (<! (async/timeout (rand-int 1000)))
        x))

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 2 (pipeline< [4 inc
                                                  1 inc
                                                  2 (fn [x]
                                                      (async/to-chan (range x)))
                                                  1 pause-rnd
                                                  3 str]
                                                 c)))))

  ;=> ["0" "1"] OK

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 2 (pipeline< [4 inc
                                                  1 inc
                                                  2 (fn [x]
                                                      (async/to-chan (range x)))
                                                  2 pause-rnd
                                                  3 str]
                                                 c)))))

  ;=> Randomly e.g. ["1" "0"] ["0" "1"] ["0" "2"] NO !!!

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 2 (pipeline< [4 inc
                                                  1 inc
                                                  2 (fn [x]
                                                      (async/to-chan (range x)))
                                                  3 pause-rnd
                                                  3 str]
                                                 c)))))

  ;=> ["1" "0"] Randomly NO (I get e.g. ["1" "3"])

  (let [c (async/chan 10)]
    (>!! c 42)
    (<!! (async/into [] (async/take 10 (pipeline< [4 inc
                                                   1 inc
                                                   2 (fn [x]
                                                       (async/to-chan (range x)))
                                                   10 pause-rnd
                                                   3 str]
                                                  c)))))

  ;=> OK!!

  )