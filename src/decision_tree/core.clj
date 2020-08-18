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


(s/def ::objective-variable string?)
(s/def ::objective-variable-vector (s/coll-of ::objective-variable))

(defn- gini-impurity
  "Calculate gini-impurity.
  `y` is objective variable vector."
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

(gini-impurity (map :Classes iris))

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

(defn- split-one-node
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
  "Calculate information-gain as all split pattern.
  Example:
    (take 3 (calculate-information-gains (create-node-from-data iris)))
    ;;=> ({:information-gain 0.1684194823599613, :threshold 5.1, :feature :Sepal.Length}
          {:information-gain 0.08546401515151525, :threshold 4.9, :feature :Sepal.Length}
          {:information-gain 0.052757793764988015, :threshold 4.7, :feature :Sepal.Length})"
  [node]
  {:pre  [(s/valid? ::node node)]
   :post [(s/valid? (s/coll-of map?) %)]}
  (->> (get-threshold-point-candidates (:data node))
       (map #(split-one-node node ((comp first vals) %1) ((comp first keys) %1)))
       (map #(hash-map
               :information-gain (information-gain (:data %) (:data (:left %)) (:data (:right %)))
               :threshold (:threshold %)
               :feature (:feature %)))))



(defn- stop-split?
  "Judge whether recursion is stop ot not. recursion is stop when
  max-depth is 0 or
  number of kinds of objective-variables is 1 or 0.
  number of kinds of objective-variables is 0 means that data is empty"
  [node max-depth]
  (or (= max-depth 0)
      (<= (count (:data node)) 1)
      (<= (count-number-of-kinds-of-objective-variables (:data node) :Classes) 1)))

(defn- get-maximum-information-gain-splitter
  "Example:
    (get-maximum-information-gain-splitter (create-node-from-data iris))
    ;;=> {:information-gain 0.33333333333333337, :threshold 0.6, :feature :Petal.Width)"
  [node]
  (->> (calculate-information-gains node)
       ;; max-key returns a last element if there are exact same values.
       ;; So we added shuffle because we want this function returns a random element in exact same values.
       shuffle
       (apply max-key :information-gain)))

(defn- get-most-popular-objective-variable-values
  [data]
  (->> data
       (map :Classes)
       (frequencies)
       (apply max-key val)
       key))

(defn- split-node
  "まえのsplit的な奴"
  [node threshold key max-depth]
  {:pre [(s/valid? ::node node)
         (s/valid? ::threshold threshold)
         (s/valid? ::feature key)]
   :post [(s/valid? ::node %)]}
  (if (stop-split? node max-depth)
    (assoc node :predict (get-most-popular-objective-variable-values (:data node)))
    (let [_ (println "node: " node)
          left-data (filter #(> (key %1) threshold) (:data node))
          left-node (create-node-from-data left-data)
          _ (println "left: " left-node)
          left-splitter (get-maximum-information-gain-splitter left-node)
          right-data (filter #(<= (key %1) threshold) (:data node))
          right-node (create-node-from-data right-data)
          _ (println "right: " right-node)
          right-splitter (get-maximum-information-gain-splitter left-node)]
      (-> node
          (update-value :feature key)
          (update-value :threshold threshold)
          (update-value :left (split-node left-node (:threshold left-splitter) (:feature left-splitter) (dec max-depth)))
          (update-value :right (split-node right-node (:threshold right-splitter) (:feature right-splitter) (dec max-depth)))))))

(defn make-decision-tree
  [train-data]
  {:pre [(s/valid? ::data train-data)]
   :post [(s/valid? ::node %)]}
  (let [node (create-node-from-data train-data)
        splitter (get-maximum-information-gain-splitter node)]
    (split-node node (:threshold splitter) (:feature splitter) 5)))

(defn predict
  "`data`は一個のデータだよ！"
  [tree data]
  (cond (not (nil? (:predict tree)))
        (:predict tree)
        (> ((:feature tree) data) (:threshold tree)) (predict (:left tree) data)
        :else (predict (:right tree) data)))


(def iris  (->> (incanter.datasets/get-dataset :iris)
                :rows
                (map (partial update-key :Species :Classes))))


(def test-data (random-sample 0.3 iris))
(def train-data (filter #(not (.contains test-data %1)) iris))

(def decision-tree (make-decision-tree train-data))
(def results
  (map #(= %1 %2)
       (->> test-data
            (map (partial predict decision-tree)))
       (->> test-data
            (map :Classes))))

"ToDo: split-nodeに下記Nodeみたいなのをいれるとrightがからになって、からをget-max-splitterに渡して留まる。調査する。
node:  {:feature nil, :threshold nil, :data ({:Sepal.Length 5.9, :Sepal.Width 3.2, :Petal.Length 4.8, :Petal.Width 1.8, :Classes versicolor} {:Sepal.Length 6.3, :Sepal.Width 2.7, :Petal.Length 4.9, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.2, :Sepal.Width 2.8, :Petal.Length 4.8, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.1, :Sepal.Width 3.0, :Petal.Length 4.9, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.0, :Sepal.Width 3.0, :Petal.Length 4.8, :Petal.Width 1.8, :Classes virginica}), :right nil, :left nil}
left:  {:feature nil, :threshold nil, :data ({:Sepal.Length 5.9, :Sepal.Width 3.2, :Petal.Length 4.8, :Petal.Width 1.8, :Classes versicolor} {:Sepal.Length 6.3, :Sepal.Width 2.7, :Petal.Length 4.9, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.2, :Sepal.Width 2.8, :Petal.Length 4.8, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.1, :Sepal.Width 3.0, :Petal.Length 4.9, :Petal.Width 1.8, :Classes virginica} {:Sepal.Length 6.0, :Sepal.Width 3.0, :Petal.Length 4.8, :Petal.Width 1.8, :Classes virginica}), :right nil, :left nil}
right:  {:feature nil, :threshold nil, :data (), :right nil, :left nil}\n
"

(println (frequencies results))
