(ns decision-tree.core
  (:require [incanter.datasets])
  (:require [clojure.spec.alpha :as s]))


(defn- update-key
  "Change `from-key` in `map` to `to-key`.
  Example:
    (def sample-map {:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Species \"setosa\"})
    (update-key :Species :Classes sample-map)
    ;;=> {:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Classes \"setosa\"}"
  [from-key to-key map]
  {:pre [(s/valid? keyword? from-key)
         (s/valid? keyword? to-key)
         (s/valid? map? map)]
   :post [(s/valid? map? %)]}
  (-> map
      (assoc to-key (from-key map))
      (dissoc from-key)))

(defn- update-value
  "Update value related to `key` in `map`.
  Example:
    (update-value :test 10 {:test nil :aiueo 5})
    ;;=> {:test 10 :aiueo 5}
  "
  [map key updated-value]
  {:pre [(s/valid? map? map)
         (s/valid? keyword? key)]
   :post [(s/valid? map? %)]}
  (update map key (fn [_] (identity updated-value))))

(def iris  (->> (incanter.datasets/get-dataset :iris)
                :rows
                (map (partial update-key :Species :Classes))))


(def test-data (random-sample 0.3 iris))
(def train-data (filter #(not (.contains test-data %1)) iris))
(def root {:feature nil :threshold nil :data train-data :right nil :left nil}) ; thresholdは以上ならleftに以下ならrightに進む．

(s/def ::objective-variable string?)
(s/def ::objective-variable-vector (s/coll-of ::objective-variable))

(defn- gini-impurity
  "`y` is objective variable vector."
  [y]
  {:pre [(s/valid? ::objective-variable-vector y)]
   :post [(s/valid? (s/and number? #(<= 0 % 1)) %)]}
  (let [number-of-each-data (->> (group-by identity y)
                                 (map (comp count val)))
        sum-of-number-of-data (apply + number-of-each-data)]
    (->> number-of-each-data
         (map #(/ % sum-of-number-of-data))
         (map #(Math/pow % 2))
         (apply +)
         (- 1))))


(defmacro debug
  "Prints args. This is useful when debugging."
  [& args]
  (let [sym (gensym "sym") val (gensym "val")]
    `(do(ns decision-tree.core
          (:require [incanter.datasets]))


        (def iris (:rows (incanter.datasets/get-dataset :iris)))

        (println "debug information:\n")
       ~@(map (fn [sym val] `(println ~sym " : " ~val)) (map str args) args)
       (println))))

(defn information-gain [node-data leaf1-data leaf2-data]
  (let [node-number-of-data (count node-data)
        node-gini-impurity (gini-impurity (map :Classes node-data))
        leaf1-number-of-data (count leaf1-data)
        leaf1-gini-impurity (gini-impurity (map :Classes leaf1-data))
        leaf2-number-of-data (count leaf2-data)
        leaf2-gini-impurity (gini-impurity (map :Classes leaf2-data))]
    (- node-gini-impurity
       (+ (* (/ leaf1-number-of-data node-number-of-data) leaf1-gini-impurity)
          (* (/ leaf2-number-of-data node-number-of-data) leaf2-gini-impurity)))))

"
(defn split-node [node max-depth]
  (if (or (= max-depth 0)
          (= (count (set (map :Classes (:data node)))) 1))
    (assoc node :predict (ffirst (sort-by val > (apply merge (map (fn [[key value]] {(keyword key) (count value)}) (group-by :Classes (:data node)))))))
    (let [features (filter #(not (= %1 :Classes)) (keys (first (:data node))))
          length-of-data (count (:data node))
          calculate-threshold-points-one-feature (fn [feature data] (let [d (sort (map feature data))
                                                                          l (unchecked-divide-int length-of-data 2)]
                                                                      ;(map #(hash-map feature (/ (+ %1 %2) 2)) (take l d) (take l (reverse d)))
                                                                      (map #(hash-map feature %) d)))
          threshold-points (flatten (map #(calculate-threshold-points-one-feature %1 (:data node)) features))
          split (fn [key, threshold] (list (filter #(> (key %1) threshold) (:data node)) (filter #(<= (key %1) threshold) (:data node))))
          information-gains (map #(apply (fn [l r] (information-gain (:data node) l r)) %1) (map #(split (first (keys %1)) ((first (keys %1)) %1)) threshold-points))
          max-information-gain-splitter (rand-nth (flatten (take 1 (partition-by :information-gain (reverse (sort-by :information-gain (map #(merge %1 {:information-gain %2} {:feature (first (keys %1))}) threshold-points information-gains)))))))
          children (map #(hash-map :data %1) (split (:feature max-information-gain-splitter) ((:feature max-information-gain-splitter) max-information-gain-splitter)))]
      (if (= (:information-gain max-information-gain-splitter) 0.0) ;全部の特徴量が等しくなって分けれない時があるので，それの対応．上で全部の特徴量がイコールというのでバリデーションしても良いかも
        (assoc node :predict (ffirst (sort-by val > (apply merge (map (fn [[key value]] {(keyword key) (count value)}) (group-by :Classes (:data node)))))))
        (assoc node :feature (:feature max-information-gain-splitter) :threshold ((:feature max-information-gain-splitter) max-information-gain-splitter) :left (split-node (first children) (dec max-depth)) :right (split-node (second children) (dec max-depth)))))))
"

(s/def ::feature (s/or :nil nil?
                       :keyword keyword?))
(s/def ::threshold (s/or :nil nil?
                         :number number?))
(s/def ::data (s/coll-of map?))
(s/def ::left (s/or :nil nil?
                    :map? ::node))
(s/def ::right (s/or :nil nil?
                     :map? ::node))
(s/def ::node  (s/keys :req-un [::feature ::threshold ::data ::left ::right]))

(defn- count-number-of-kinds-of-objective-variables
  "Example:
    (count-number-of-kinds-of-objective-variables iris :Classes)
    ;;=> 3"
  [data key-of-objective-variable]
  {:pre [(s/valid? ::data data)
         (s/valid? keyword? key-of-objective-variable)]
   :post [(s/valid? int? %)]}
  (->> data
       (map key-of-objective-variable)
       set
       count))

(defn- get-explanatory-variables-from
  "Example:
    (get-explanatory-variables-from iris :Classes)
    ;;=> (:Sepal.Length :Sepal.Width :Petal.Length :Petal.Width)"
  [data key-of-objective-variable]
  {:pre [(s/valid? ::data data)
         (s/valid? keyword? key-of-objective-variable)]
   :post [s/valid? (s/coll-of keyword?) %]}
  (->> (first data)
       keys
       (filter (partial not= key-of-objective-variable))))


(defn- get-threshold-point-candidates
  "Get candidates of threshold to split node to right node and left node.

  Now implementation of this function is that the function takes data
  then returns coll of map of explanatory-variable and the value.
  Example:
    (take 3 iris)
    ;;=>({:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Classes \"setosa\"}
         {:Sepal.Length 4.9, :Sepal.Width 3.0, :Petal.Length 1.4, :Petal.Width 0.2, :Classes \"setosa\"}
         {:Sepal.Length 4.7, :Sepal.Width 3.2, :Petal.Length 1.3, :Petal.Width 0.2, :Classes \"setosa\"})
    (get-threshold-point-candidates (take 3 iris))
    ;;=> ({:Sepal.Length 5.1}
          {:Sepal.Length 4.9}
          {:Sepal.Length 4.7}
          {:Sepal.Width 3.5}
          {:Sepal.Width 3.0}
          {:Sepal.Width 3.2}
          {:Petal.Length 1.4}
          {:Petal.Length 1.4}
          {:Petal.Length 1.3}
          {:Petal.Width 0.2}
          {:Petal.Width 0.2}
          {:Petal.Width 0.2})"
  [data]
  {:pre [(s/valid? ::data data)]
   :post [(s/valid? (s/coll-of map?) %)]}
  (let [features (get-explanatory-variables-from data :Classes)
        get-threshold-point-candidates-one-feature (fn [data feature] (map #(select-keys %1 [feature]) data))]
    (->> (map (partial get-threshold-point-candidates-one-feature data) features)
         flatten)))

(defn- create-node-from-data
  [data]
  {:pre [(s/valid? ::data data)]
   :post [s/valid? ::node %]}
  {:feature nil :threshold nil :data data :right nil :left nil})

(defn- split-node
  "まえのsplit的な奴"
  [node threshold key]
  {:pre [(s/valid? ::node node)
         (s/valid? ::threshold threshold)
         (s/valid? ::feature key)]
   :post [(s/valid? ::node %)]}
  (let [left-data (filter #(> (key %1) threshold) (:data node))
        left-node (create-node-from-data left-data)
        right-data (filter #(<= (key %1) threshold) (:data node))
        right-node (create-node-from-data right-data)]
    (-> node
        (update-value :feature key)
        (update-value :threshold threshold)
        (update-value :left left-node)
        (update-value :right right-node))))

(defn- calculate-information-gains
  ""
  [node]
  (->> (get-threshold-point-candidates (:data node))
       (map #(split-node node ((comp first vals) %1) ((comp first keys) %1)))
       (map #(information-gain (:data node) (:data (:left %)) (:data (:right %))))))

(calculate-information-gains (create-node-from-data iris))

(defn split-node2
  [node max-depth]
  {:pre [(s/valid? ::node node)
         (s/valid? int? max-depth)]
   :post [(s/valid? ::node %)]})

(defn predict
  "`data`は一個のデータだよ！"
  [tree data]
  (cond (not (nil? (:predict tree)))
        (:predict tree)
        (> ((:feature tree) data) (:threshold tree)) (predict (:left tree) data)
        :else (predict (:right tree) data)))


(defn make-decision-tree [train-data]
  (let [root {:feature nil :threshold nil :data train-data :right nil :left nil}
        max-depth 3]
    (split-node root max-depth)))
(def decision-tree (make-decision-tree train-data))
(def correct-percent (map #(= %1 %2) (map #(predict decision-tree %1) test-data) (map keyword (map :Classes test-data))))

(println correct-percent)
