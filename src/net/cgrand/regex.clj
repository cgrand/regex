(ns net.cgrand.regex
  "A DSL for people who prefer verbose, maintenable regexes to terse 
   now-you-have-two-problems ones."
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [repeat + * resolve derive])
  (:require [clojure.core :as core]
            [clojure.string :as s]
            [net.cgrand.regex.charset :as cs])
  (:import [java.util.regex Pattern]))

;; Value-based DSL definition
(defprotocol RegexValue
  (pattern [this] "Returns the pattern represented by this value.")
  (groupnames [this] "Returns a seq of all group names used in this regex.")
  (match-empty? [this])
  (firsts [this] "Returns a collection of disjunct charsets -- for a given 
    charset, derive must be constant.")
  (derive [this x]))

(declare regex*)

(defrecord Regex [^Pattern re groupnames spec]
  RegexValue
    (pattern [this] (.pattern re))
    (groupnames [this] groupnames)
    (match-empty? [this] (match-empty? spec))
    (firsts [this] (firsts spec))
    (derive [this x]
      (when-let [spec (derive spec x)]
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

(extend-protocol RegexValue
  Character
    (pattern [c]
      (pattern (str c)))
    (groupnames [this] [])
    (match-empty? [this] false)
    (firsts [c] [(cs/charset c)])
    (derive [c x]
      (when (= c x) ""))
  String ; a String denotes a literal sequence of characters to match 
    (pattern [s]
      (Pattern/quote s))
    (groupnames [this] [])
    (match-empty? [s] (= "" s))
    (firsts [s] [(cs/charset (first s))])
    (derive [s c]
      (when (= c (first s)) (subs s 1)))
  clojure.lang.IPersistentSet ; a Set denotes an alternative
    (pattern [set]
      (str "(?:" (s/join "|" (map pattern set)) ")"))
    (groupnames [set] 
      (mapcat groupnames set))
    (match-empty? [set] (some match-empty? set))
    (firsts [set]
      (reduce cs/disjunctive-union
              (map firsts set)))
    (derive [s c]
      (when-let [[x & xs :as ds] (seq (keep #(derive % c) s))]
        (if xs (set ds) x)))
  clojure.lang.ISeq ; a seq denotes a non-capturing group
    (pattern [v]
      (s/join (map pattern v)))
    (groupnames [v] 
      (mapcat groupnames v))
    (match-empty? [v] (every? match-empty? v))
    (firsts [s]
      (when-let [[x & xs] (seq s)]
        (if (match-empty? x)
          (firsts #{x xs})
          (firsts x))))
    (derive [s c]
      (when-let [[x & xs] (seq s)]
        (let [dx (derive x c)
              dxs (when (and xs (match-empty? x)) (derive xs c))
              dxxs (when dx (if xs (cons dx xs) dx))]
          (or
            ; I have a nagging doubt that the set below may throw a duplicate
            ; key exception without the not= 
            (and dxxs dxs (not= dxxs dxs) #{dxxs dxs}) 
            dxxs dxs))))
  clojure.lang.IPersistentVector ; a Vector denotes a group (capturing or not)
    (pattern [v]
      (if (-> v rseq second (= :as)) 
        (str "(" (pattern (-> v pop pop seq)) ")")
        (pattern (seq v))))
    (groupnames [v] 
      (if (-> v rseq second (= :as))
        (cons (peek v) (groupnames (-> v pop pop seq)))
        (groupnames (seq v))))
    (match-empty? [v] 
      (match-empty? (as-seq v)))
    (firsts [v] 
      (firsts (seq (as-seq v))))
    (derive [v c]
      (derive (as-seq v) c))
  clojure.lang.APersistentMap ; a map denotes a char range
    (pattern [m] (pattern (cs/charset m)))
    (groupnames [v] [])
    (match-empty? [this] false)
    (firsts [m] (firsts (cs/charset m)))
    (derives [m c] (derive (cs/charset m) c))
  net.cgrand.regex.charset.Charset
    (pattern [cs]
      (let [reserved (set "[]&^-")
            esc #(if (and (not (< 0x1F (int %) 0x7F)) (reserved %))
                   %
                   (format "\\u%04X" (int %)))
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
    (firsts [this] [this])
    (derives [this c]
      (when (cs/has? this c) ""))
  nil
    (pattern [_] "(?!a)(?=a)")
    (groupnames [_] [])
    (match-empty? [_] false)
    (firsts [_] nil)
    (derive [_ c] nil))

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
    (firsts [this] (firsts frag))
    (derive [this c]
      (when-let [dfrag (derive frag c)]
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