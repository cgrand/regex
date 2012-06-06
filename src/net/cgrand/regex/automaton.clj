(ns net.cgrand.regex.automaton
  (:refer-clojure :exclude [derive complement])
  (:require [net.cgrand.regex.charset :as cs]))

(defprotocol State
  (firsts [this] "Returns a collection of disjunct charsets -- for a given 
    charset, derive must be constant.")
  (derive [this x])
  (accept? [this]))

(extend-type nil
  State
    (accept? [this] false)
    (firsts [_] nil)
    (derive [_ c] nil))

(defn dfa [state]
  (loop [transitions {} todo #{state}]
    (if-let [[x] (seq todo)]
      (let [rhs (reduce (fn [rhs cs]
                          (let [y (when-let [c (cs/pick cs)] (derive x c))]
                            (update-in rhs [y] cs/+ cs))) 
                  {} (firsts x))
            xs (keys rhs)
            rhs (if (accept? x) (assoc rhs ::accept []) rhs)
            transitions (assoc transitions x rhs)
            todo (-> todo
                   (disj x)
                   (into (remove transitions xs)))]
        (recur transitions todo))
      (assoc transitions ::S (transitions state)))))

(defn complement [dfa]
  (into {::F {::accept [] ::F cs/any-char}}
    (for [[x rhs] dfa]
      (let [rhs (if (::accept rhs) (dissoc rhs ::accept) rhs)
            other-chars (reduce cs/- cs/any-char (vals (dissoc rhs ::accept)))
            rhs (if (cs/pick other-chars) (assoc rhs ::F other-chars) rhs)]
        [x rhs]))))

(defrecord DFAState [dfa x]
  State
  (firsts [this]
    (vals (dissoc (dfa x) ::accept)))
  (derive [this c]
    (when-let [y (some (fn [[x cs]] (when (cs/has? cs c) x))
                       (dissoc (dfa x) ::accept))]
      (DFAState. dfa y)))
  (accept? [this]
    (boolean (::accept (dfa x)))))

(defn dfa-state
  ([dfa] (dfa-state dfa ::S))
  ([dfa x]
    (DFAState. dfa x)))

(defrecord UnionState [states]
  State
  (firsts [this]
    (reduce cs/disjunctive-union (map firsts states)))
  (derive [this x]
    (UnionState. (keep #(derive % x) states)))
  (accept? [this]
    (some accept? states)))

(defn union [& dfas]
  (dfa (UnionState. (map dfa-state dfas))))

(defrecord IntersectionState [states]
  State
  (firsts [this]
    (reduce cs/disjunctive-intersection (map firsts states)))
  (derive [this x]
    (IntersectionState. (keep #(derive % x) states)))
  (accept? [this]
    (every? accept? states)))

(defn intersection [& dfas]
  (dfa (IntersectionState. (map dfa-state dfas))))

