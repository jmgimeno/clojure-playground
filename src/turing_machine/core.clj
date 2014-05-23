(ns turing-machine.core
  (:refer-clojure :exclude [read write]))

(defn make-tape
  ([] (make-tape [0]))
  ([[x & xs]] {:left [] :current x :right xs}))

(defn left [{[l & ls] :left c :current r :right}]
  {:left ls :current (or l 0) :right (cons c r)})

(defn right [{l :left c :current [r & rs] :right}]
  {:left (cons c l) :current (or r 0) :right rs})

(defn read [{c :current}] c)

(defn write [tape new]
  (assoc tape :current new))

(defn erase [taoe]
  (write tape 0))

;; The transition table should be a matrix of
;; (State, Symbol) => (State, Symbol, Move)

(defn make-table [] {})

(defn add-transition [table state symbol newState newSymbol move]
  (assoc table
    {:state state :symbol symbol}
    {:state newState :symbol newSymbol :move move}))


