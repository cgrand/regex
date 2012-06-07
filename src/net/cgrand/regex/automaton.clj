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

(def always {::S {::S cs/any-char ::accept []}})

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

(defrecord ComplementState [dfa x]
  State
  (firsts [this]
    (let [css (vals (dissoc (dfa x) ::accept))
          ccs (reduce cs/- cs/any-char css)]
      (cons ccs css)))
  (derive [this c]
    (if-let [y (some (fn [[x cs]] (when (cs/has? cs c) x))
                       (dissoc (dfa x) ::accept))]
      (ComplementState. dfa y)
      (dfa-state always)))
  (accept? [this]
    (not (::accept (dfa x)))))

(defn complement [dfb]
  (dfa (ComplementState. dfb ::S)))

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

