(ns cljnect4wss.utils.prelude
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as g]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as string]
   [clojure.edn :as edn])
  (:gen-class))

(defn not-nil? [x] (not (nil? x)))

(defn gen-spec
  "Convienence function for directly getting a generation of a given spec."
  [spec]
  (-> spec s/gen g/generate))

(defn gen-sample
  "Convienence function for directly getting a sample of a given spec."
  [spec]
  (-> spec s/gen g/sample))

;(def read-ednfile (comp edn/read-string slurp))

(defn if-comp [comparison values then else]
  (if (apply comparison values) then else))
(def if-eq (partial if-comp =))

(defn all?
  "Returns true if pred returns true for all values."
  [pred values]
  (eval `(and ~@(map pred values))))

(defn if-all [pred values then else]
  (if (all? pred values) then else))

(defn when-all [pred values then]
  (if-all pred values then nil))

(defn map-if
  "If pred evals to something truthy, map function f to collection c,
  otherwise, merely return c."
  [pred f c]
  (if pred (map f c) c))

(defn update-if
  "If pred evals to something truthy, update key k of map m according to
  function f, otherwise, merely return m."
  [pred m k f]
  (if pred (update m k f) m))

(def none (complement some))

(defn in? [coll x]
  (some #(= x %) coll))

(def not-in? (complement in?))

(defn filter-in [in-coll subject-coll]
  (filter #(in? in-coll %) subject-coll))

(defn filter-out [in-coll subject-coll]
  (filter #(not-in? in-coll %) subject-coll))

(def to-pairs (partial into []))
(def to-map (partial into {}))

(defn positional-apply
  "((positional-apply inc dec) [1 1]) ; => [2 0]"
  [& funs]
  (partial map #(%1 %2) funs))

(defn merge-if [pred coll & colls]
  (if pred
    (apply merge (concat coll colls))
    coll))

(defn merge-when-exists [x] (partial merge-if (not-nil? x)))

(defn has-keys?
  [coll key-list]
  (every? #(contains? coll %) key-list))

(def access #(partial get-in %))

(defn uncons [l]
  [(first l) (rest l)])

(defn on-kvs [operation f m]
  (->> m to-pairs (operation f) to-map))

(def map-kvs (partial on-kvs map))
(def filter-kvs (partial on-kvs filter))

(defn map-keys [f m]
  (let [[ks vs] [(keys m) (vals m)]
        new-keys (map f ks)]
    (zipmap new-keys vs)))

(defn filter-vals [f m]
  (select-keys m (for [[k v] m :when (f v)] k)))

(defn map-vals [f m]
  (let [[ks vs] [(keys m) (vals m)]
        new-vals (map f vs)]
    (zipmap ks new-vals)))

(defn map->nsmap
  [m n]
  (reduce-kv (fn [acc k v]
               (let [new-kw (if (and (keyword? k)
                                     (not (qualified-keyword? k)))
                              (keyword (str n) (name k))
                              k)]
                 (assoc acc new-kw v)))
             {} m))

(defn map->map-list-with-assocd-indexes
  "Given map M of the format
    {1 {:myKey x}
     2 {:myKey y}}
  and a key-name arg of :idKey, return
    [{:idKey 1 :myKey x}
     {:idKey 2 :myKey y}]"
  [m key-name]
  (reduce-kv
    (fn [acc k v]
      (conj acc (assoc v key-name k)))
    []
    m))

(defn confirm-if
  "Takes a set of test pairs, [t1 t2]. For each pair, if t1 evaluates to true,
  and t2 does not evaluate to true, the function returns false. All other
  evaluation possibilites return true.

  Read as: 'Confirm that if A is true, B is also true'."
  [& cond-form-pairs]
  (when (-> cond-form-pairs count odd?)
    (throw
      (Exception. "confirm-if expects an even number of arguments")))

  (let [pairs (partition 2 cond-form-pairs)]
    (boolean (reduce
               (fn [acc [condition check]]
                 (and acc (or (not condition) check)))
               true
               pairs))))

