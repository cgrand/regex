(ns net.cgrand.regex.charset
  (:refer-clojure :exclude [complement * + - not contains?]))

(defprotocol Rangeable
  (ranges [cs]))

(defprotocol Charsetable
  (charset [x]))

(extend-protocol Rangeable
  String
  (ranges [s] (map vector s s))
  Character
  (ranges [c] [[c c]])
  java.lang.Number
  (ranges [n] [[n n]])
  clojure.lang.APersistentMap
  (ranges [m] (seq m))
  clojure.lang.APersistentSet
  (ranges [s] (map (fn [x] [x x]) s))
  nil
  (ranges [_] nil))

(defrecord Charset [cs]
  Rangeable
  (ranges [_] (seq cs))
  Charsetable
  (charset [this] this))

(defn lt [[a b] [c d]]
  (boolean (and b c (< (int b) (int c)))))

(def no-char (Charset. (sorted-set-by lt)))

(extend-protocol Charsetable
  nil
  (charset [_]
    no-char))

(defn- pred [c]
  (when (and c (pos? (int c))) (char (dec (int c)))))

(defn- succ [c]
  (when (and c (< (int c) 0xFFFF)) (char (inc (int c)))))

(defn- split 
  "Splits ranges right after x."
  [cs x]
  (if-let [[a b :as r] (when x (get cs [x x]))]
    (if (or (= b x) (and b x (= (int b) (int x))))
      cs
      (-> cs (disj r) (conj [a x] [(succ x) b])))
    cs))

(defn- between [rs a b]
  (cond
    (and a b) (subseq rs >= [a a] <= [b b])
    a (subseq rs >= [a a])
    b (subseq rs <= [b b])
    :else (seq rs)))

(defn- subtract [cs [a b]]
  (let [rs (-> cs :cs (split (pred a)) (split b))]
    (Charset. (reduce disj rs (between rs a b)))))

(defn- add [cs [a b]]
  (let [rs (:cs cs)
        aa (pred a)
        bb (succ b)
        a (when a (first (get rs [aa aa] [a a])))
        b (when b (second (get rs [bb bb] [b b])))]
    (Charset. (conj (reduce disj rs (between rs a b))
                    [a b]))))

(def any-char (add no-char [nil nil]))

(extend-protocol Charsetable
  Object
  (charset [x]
    (reduce add no-char (ranges x))))

(defn + "union"
  ([] no-char)
  ([a] (charset a))
  ([a b]
    (reduce add (charset a) (ranges b)))
  ([a b & cs] 
    (reduce + (+ a b) cs)))

(defn - "complement or asymetric difference"
  ([x] (reduce subtract any-char (ranges x)))
  ([x & xs]
    (reduce #(reduce subtract %1 (ranges %2)) x xs)))

(defn * "intersection"
  ([] any-char)
  ([a] (charset a))
  ([a b]
    (- (+ (- a) (- b))))
  ([a b & cs]
    (- (reduce + (+ (- a) (- b)) (map - cs)))))

(defn not [& xs]
  (- (reduce + xs)))

(defn pick 
  "Returns a character contained in the charset or nil if the
   charset is empty."
  [cs]
  (when-let [[a b] (first (ranges cs))]
    (or a \u0000)))

(defn has? [cs c]
  (boolean ((:cs (charset cs)) [c c])))

(defn disjunctive-union
  "as and bs are collection of disjunct charsets, returns their union as a
   collection of smaller disjunct charsets." 
  ([] nil)
  ([as] as)
  ([as bs]
    (let [A (reduce + as)
          B (reduce + bs)]
      (filter pick
        (concat
          (map #(- % B) as)
          (map #(- % A) bs)
          (for [a as b bs] (* a b)))))))

(defn disjunctive-intersection
  "as and bs are collection of disjunct charsets, returns their intersection
   as a collection of smaller disjunct charsets." 
  ([] [any-char])
  ([as] as)
  ([as bs]
    (filter pick
      (for [a as b bs] (* a b)))))
