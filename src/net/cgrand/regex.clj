(ns net.cgrand.regex
  "A DSL for people who prefer verbose, maintenable regexes to terse 
   now-you-have-two-problems ones."
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [repeat + * resolve derive])
  (:require [clojure.core :as core]
            [clojure.string :as s]
            [net.cgrand.regex.charset :as cs]
            [net.cgrand.regex.automaton :as dfa])
  (:import [java.util.regex Pattern]))

;; Value-based DSL definition
(defprotocol RegexValue
  (pattern [this] "Returns the pattern represented by this value.")
  (groupnames [this] "Returns a seq of all group names used in this regex.")
  (match-empty? [this]))

(declare regex*)

(defrecord Regex [^Pattern re groupnames spec]
  RegexValue
    (pattern [this] (.pattern re))
    (groupnames [this] groupnames)
    (match-empty? [this] (match-empty? spec))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [this] (dfa/firsts spec))
    (derive [this x]
      (when-let [spec (dfa/derive spec x)]
        (regex* spec))))

(defn exec [re s]
  (when-let [res (re-matches (:re re) s)]
    (if-let [ids (seq (:groupnames re))]
      (zipmap (cons nil ids) res)
      res)))

(defn regex* [spec]
  (Regex. (-> spec pattern Pattern/compile) (groupnames spec) spec))

(defn- as-seq [v]
  (sequence (if (-> v rseq second (= :as))
              (-> v pop pop)
              v)))

(extend-type Character
  RegexValue
    (pattern [c]
      (pattern (str c)))
    (groupnames [this] [])
    (match-empty? [this] false)
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [c] [(cs/charset c)])
    (derive [c x]
      (when (= c x) "")))

(extend-type String ; a String denotes a literal sequence of characters to match 
  RegexValue
    (pattern [s]
      (Pattern/quote s))
    (groupnames [this] [])
    (match-empty? [s] (= "" s))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [s] (when-let [[c] (seq s)] [(cs/charset c)]))
    (derive [s c]
      (when (= c (first s)) (subs s 1))))

(extend-type clojure.lang.APersistentSet ; a Set denotes an alternative
  RegexValue
    (pattern [set]
      (str "(?:" (s/join "|" (map pattern set)) ")"))
    (groupnames [set] 
      (mapcat groupnames set))
    (match-empty? [set] (some match-empty? set))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [set]
      (reduce cs/disjunctive-union
              (map dfa/firsts set)))
    (derive [s c]
      (when-let [[x & xs :as ds] (seq (keep #(dfa/derive % c) s))]
        (if xs (set ds) x))))

(extend-type clojure.lang.ISeq ; a seq denotes a non-capturing group
  RegexValue
    (pattern [v]
      (s/join (map pattern v)))
    (groupnames [v] 
      (mapcat groupnames v))
    (match-empty? [v] (every? match-empty? v))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [s]
      (when-let [[x & xs] (seq s)]
        (if (match-empty? x)
          (dfa/firsts #{x xs})
          (dfa/firsts x))))
    (derive [s c]
      (when-let [[x & xs] (seq s)]
        (let [dx (dfa/derive x c)
              dxs (when (and xs (dfa/accept? x)) (dfa/derive xs c))
              dxxs (when dx (if xs (cons dx xs) dx))]
          (or
            ; I have a nagging doubt that the set below may throw a duplicate
            ; key exception without the not= 
            (and dxxs dxs (not= dxxs dxs) #{dxxs dxs}) 
            dxxs dxs)))))

(extend-type clojure.lang.APersistentVector ; a Vector denotes a group (capturing or not)
  RegexValue
    (pattern [v]
      (if (-> v rseq second (= :as)) 
        (str "(" (pattern (as-seq v)) ")")
        (pattern (as-seq v))))
    (groupnames [v] 
      (if (-> v rseq second (= :as))
        (cons (peek v) (groupnames (as-seq v)))
        (groupnames (as-seq v))))
    (match-empty? [v] 
      (match-empty? (as-seq v)))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [v] 
      (dfa/firsts (seq (as-seq v))))
    (derive [v c]
      (dfa/derive (as-seq v) c)))

(extend-type clojure.lang.APersistentMap ; a map denotes a char range
  RegexValue
    (pattern [m] (pattern (cs/charset m)))
    (groupnames [v] [])
    (match-empty? [this] false)
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [m] (dfa/firsts (cs/charset m)))
    (derive [m c] (dfa/derive (cs/charset m) c)))

(extend-type net.cgrand.regex.charset.Charset
  RegexValue
    (pattern [cs]
      (let [reserved (set "[]&^-")
            esc #(if (or (not (< 0x1F (int %) 0x7F)) (reserved %))
                   (format "\\u%04X" (int %)) 
                   (char %))
            cs (-> cs cs/charset cs/ranges)]
        (apply str (concat ["["] 
                           (mapcat (fn [[a b]]
                                     (if (and a (= a b))
                                       [(esc a)]
                                       [(esc (or a \u0000)) "-" 
                                        (esc (or b \uFFFF))])) cs) 
                           ["]"]))))
    (groupnames [v] [])
    (match-empty? [this] false)
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [this] [this])
    (derive [this c]
      (when (cs/has? this c) "")))

#_(extend-type nil
  RegexValue
    (pattern [_] "(?=X)(?!X)")
    (groupnames [_] [])
    (match-empty? [_] false))

(defn regex [& specs] 
  (regex* (vec specs)))

(defrecord Repeat [frag min max]
  RegexValue
    (pattern [this] 
      (let [s (pattern frag)
            max (or max "")]
        (str "(?:" s "){" min "," max "}")))
    (groupnames [this] 
      (groupnames frag))
    (match-empty? [this] (or (zero? min) (match-empty? frag)))
  dfa/State
    (accept? [this] (match-empty? this))
    (firsts [this] (dfa/firsts frag))
    (derive [this c]
      (when-let [dfrag (dfa/derive frag c)]
        (cond
          (and max (< min max)) (list dfrag (Repeat. frag min (dec max)))
          (= min max) dfrag
          (nil? max) (list dfrag this)))))

(defn repeat 
 ([spec] (Repeat. spec 0 nil))  
 ([spec min] (Repeat. spec min nil))
 ([spec min max] (Repeat. spec min max)))

(defn *
 [& specs]
  (repeat (vec specs)))

(defn + 
 [& specs]
  (repeat (vec specs) 1))

(defn ?
 [& specs]
  (repeat (vec specs) 0 1))

(def any cs/any-char)

(defrecord PositiveLookahead [frag]
  RegexValue
  (pattern [this]
    (str "(?=" (pattern frag) ")"))
  (groupnames [this]
    (groupnames frag)))

(defn ?= [frag] (PositiveLookahead. frag))

(defrecord NegativeLookahead [frag]
  RegexValue
  (pattern [this]
    (str "(?!" (pattern frag) ")"))
  (groupnames [this]
    nil))

(defn ?! [frag] (NegativeLookahead. frag))

;; predefined classes
(def digit {\0 \9})
(def !digit (cs/- digit))
(def space (cs/charset " \t\n\u000B\f\r"))
(def !space (cs/- space))
(def wordchar {\a \z \A \Z \_ \_ \0 \9})
(def !wordchar (cs/- wordchar))

(defmacro letmap [& bindings]
  `(let [~@bindings]
     ~(let [syms (take-nth 2 bindings)]
        (zipmap (map keyword syms) syms))))

(def posix
  (letmap 
    Lower {\a \z}
    Upper {\A \Z}
    ASCII {\u0000 \u007F}
    Alpha (cs/+ Lower Upper)
    Digit {\0 \9}
    Alnum (cs/+ Alpha Digit)
    Punct (cs/charset "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
    Graph (cs/+ Alnum Punct)
    Print (cs/+ Graph \space)
    Blank (cs/charset " \t")
    Cntrl (cs/+ {\u0000 \u001F} \u007F)
    XDigit {\0 \9 \a \f \A \F}
    Space (cs/charset " \t\n\u000B\f\r")))

;; automaton to spec
(defn- cat [a b]
  (if (vector? a)
    (if (vector? b)
      (into a b)
      (conj a b))
    (if (vector? b)
      (into [a] b)
      [a b])))

(defn- either [a b]
  (if (set? a)
    (if (set? b)
      (into a b)
      (conj a b))
    (cond
      (set? b) (into b a)
      (= a b) a
      :else #{a b})))

(defn- solve 
  "If the equation is recursive, apply Arden's rule."
  [x rhs]
  (if-let [seg (rhs x)]
    (into {} 
      (for [[s re] (dissoc rhs x)]
        [s (cat (repeat seg) re)]))
    rhs))

(defn either-cat [a b1 b2]
  (if a (either a (cat b1 b2)) (cat b1 b2)))

(defn- substitute1 [yrhs x xrhs]
  (if-let [re (yrhs x)]
    (reduce (fn [yrhs [z zre]] (update-in yrhs [z] either-cat re zre)) 
      (dissoc yrhs x) xrhs)
    yrhs))

(defn- substitute [equations x]
  (let [rhs (apply solve (find equations x))]
    (into {}
      (for [[y yrhs] (dissoc equations x)]
        [y (substitute1 yrhs x rhs)]))))

(defn dfa-to-spec [dfa]
  (let [eqs (reduce substitute dfa (keys (dissoc dfa ::dfa/S)))]
    (::dfa/accept (first (vals eqs)))))


(comment 
  regex=> (exec (regex [(repeat {\a \z}) :as :a] " " [(repeat {\a \z}) :as :b]) 
            "hello world")
  {:b "world", :a "hello", nil "hello world"}
  
  (def datestamp-re 
    (let [d {\0 \9}]
      (regex [d d d d :as :year] \- [d d :as :month] \- [d d :as :day])))
  regex=> (exec datestamp-re "2007-10-23")
  {:day "23", :month "10", :year "2007", nil "2007-10-23"}
  regex=> (exec datestamp-re "20X7-10-23")
  nil
)