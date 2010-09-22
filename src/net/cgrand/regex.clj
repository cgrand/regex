(ns net.cgrand.regex
  "A DSL for people who prefer verbose, maintenable regexes to terse 
   now-you-have-two-problems ones."
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [repeat + * resolve])
  (:require [clojure.core :as core]
            [clojure.string :as s])
  (:import [java.util.regex Pattern]))

;; Value-based DSL definition
(defprotocol RegexValue
  (pattern [this] "Returns the pattern represented by this value.")
  (groupnames [this] "Returns a seq of all group names used in this regex."))

(defrecord Regex [^Pattern re groupnames]
  RegexValue
    (pattern [this] (.pattern re))
    (groupnames [this] groupnames))

(defn exec [re s]
  (when-let [res (re-matches (:re re) s)]
    (if-let [ids (seq (:groupnames re))]
      (zipmap (cons nil ids) res)
      res)))

(defn regex* [spec]
  (Regex. (-> spec pattern Pattern/compile) (groupnames spec)))

(extend-protocol RegexValue
  Character
    (pattern [c]
      (pattern (str c)))
    (groupnames [this] [])
  String ; a String denotes a literal sequence of characters to match 
    (pattern [s]
      (Pattern/quote s))
    (groupnames [this] [])
  clojure.lang.IPersistentSet ; a Set denotes an alternative
    (pattern [set]
      (str "(?:" (s/join "|" (map pattern set)) ")"))
    (groupnames [set] 
      (mapcat groupnames set))
  clojure.lang.ISeq ; a seq denotes a non-capturing group
    (pattern [v]
      (s/join (map pattern v)))
    (groupnames [v] 
      (mapcat groupnames v))
  clojure.lang.IPersistentVector ; a Vector denotes a group (capturing or not)
    (pattern [v]
      (if (-> v rseq second (= :as)) 
        (str "(" (pattern (-> v pop pop seq)) ")")
        (pattern (seq v))))
    (groupnames [v] 
      (if (-> v rseq second (= :as))
        (cons (peek v) (groupnames (-> v pop pop seq)))
        (groupnames (seq v))))
  clojure.lang.IPersistentMap ; a map denotes a char range
    (pattern [m]
      (let [esc {\] "\\x5D" \[ "\\x5B" \& "\\x26" \^ "\\x5E"}]
        (apply str (concat ["["] 
                     (mapcat (fn [[k v]]
                               [(esc k k) \- (esc v v)]) m) ["]"]))))
    (groupnames [v] [])
  ; I haven't found a way to get the number of groups of a given pattern
  #_Pattern
    #_(pattern [re] (.pattern re))
    #_(groupnames [re] []))

(defn regex [& specs] 
  (regex* (vec specs)))

(defrecord Repeat [frag min max]
  RegexValue
    (pattern [this] 
      (let [s (pattern frag)
            max (or max "")
            min (or min 0)]
        (str "(?:" s "){" min "," max "}")))
    (groupnames [this] 
      (groupnames frag)))

(defn repeat 
 ([spec] (Repeat. spec nil nil))  
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

(def any 
  (reify 
    RegexValue
      (pattern [this] ".")
      (groupnames [this] [])))

;; at this point the DSL is fully functional and everything else is just 
;; compile-time optimizations -- optimizations that can be disabled by setting
;; *optimize* to false

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

;; Compile-time simplification/optimization
(def *optimize* true)

(defprotocol RegexFragment
  (static? [this env] "Returns true when the fragment is static.")
  (recursimplify [this env]))

(defn- resolve [sym env]
  (when-not (contains? env sym) (core/resolve sym)))

(defn- simplify [expr env]
  (if (static? expr env)
    (let [{:keys [re groupnames]} (eval `(regex* ~expr))]
      `(Regex. ~re ~(vec groupnames)))
    (recursimplify expr env)))

(extend-protocol RegexFragment
  Character 
    (static? [this _] true)
  String 
    (static? [this _] true)
  clojure.lang.IPersistentSet
    (static? [this env] (every? #(static? % env) this))
    (recursimplify [this env] (set (map #(simplify % env) this)))
  clojure.lang.IPersistentVector
    (static? [this env] (every? #(static? % env) this))
    (recursimplify [this env] (vec (map #(simplify % env) this)))
  clojure.lang.IPersistentMap
    (static? [m _] (every? (fn [[k v]] (and (char? k) (char? v))) m))
    (recursimplify [this _] this)
  clojure.lang.Keyword 
    (static? [this _] true)
  clojure.lang.Symbol
    (static? [this env] (-> this (resolve env) meta ::static))
    (recursimplify [this _] this)
  clojure.lang.ISeq 
    (static? [this env]
      (let [[f & xs] this]
        (when-let [static-args? (and (symbol? f) 
                                  (-> f (resolve env) meta ::static-args?))]
          (static-args? xs env))))
    (recursimplify [this env]
      (let [[f] this]
        (when-let [simplify (and (symbol? f) 
                              (-> f (resolve env) meta ::simplify))]
          (simplify this env))))
  #_Pattern
    #_(static? [this _] true)
  Object
    (static? [this _] false)
    (recursimplify [this _] this))

(doseq [v [#'regex #'* #'+ #'?]]
  (alter-meta! v merge {::static-args? #(static? (vec %1) %2)
                        ::simplify (fn [[f & xs] env]
                                     (cons f (map #(simplify % env) xs)))}))

(alter-meta! #'any merge {::static true})

(alter-meta! #'repeat merge 
  {::static-args? (fn [[spec & opts] env]
                    (and (static? spec env) (every? number? opts)))
   ::simplify (fn [[f spec & opts] env] (list* f (simplify spec env) opts))})

(defmacro simplify-regex [spec]
  (if (and *optimize* (static? spec &env))
    (let [re (regex* (eval spec))]
      `(Regex. ~(:re re) ~(vec (:groupnames re))))
    `(regex* ~spec)))

(alter-meta! #'regex merge
  {:inline (fn [& specs] `(simplify-regex [~@specs]))})


