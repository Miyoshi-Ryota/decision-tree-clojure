(ns decision-tree.core
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
  "Split node to make decision-tree."
  [node threshold key max-depth]
  {:pre [(s/valid? ::node node)
         (s/valid? ::threshold threshold)
         (s/valid? ::feature key)]
   :post [(s/valid? ::node %)]}
  (if (stop-split? node max-depth)
    (assoc node :predict (get-most-popular-objective-variable-values (:data node)))
    (let [split (split-one-node node threshold key)
          left-node (:left split)
          right-node (:right split)
          left-splitter (get-maximum-information-gain-splitter left-node)
          right-splitter (get-maximum-information-gain-splitter right-node)]
      (-> node
          (update-value :feature key)
          (update-value :threshold threshold)
          (update-value :left (split-node left-node (:threshold left-splitter) (:feature left-splitter) (dec max-depth)))
          (update-value :right (split-node right-node (:threshold right-splitter) (:feature right-splitter) (dec max-depth)))))))

(defn make-decision-tree
  "Make decision-tree.
  `train-data` is coll of map.
  `max-depth` is max-depth of decision-tree made
  `key-of-objective-variable` is key-of-objective-variable for example if use iris dataset this is :Species
  Example of `make-decision-tree` used iris dataset :
    (take 3 iris)
    ;; =>({:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Species \"setosa\"}
          {:Sepal.Length 4.9, :Sepal.Width 3.0, :Petal.Length 1.4, :Petal.Width 0.2, :Species \"setosa\"}
          {:Sepal.Length 4.7, :Sepal.Width 3.2, :Petal.Length 1.3, :Petal.Width 0.2, :Species \"setosa\"}
    (def decision-tree (make-decision-tree iris 3 :Species)
  "
  [train-data max-depth key-of-objective-variable]
  {:pre [(s/valid? ::data train-data)]
   :post [(s/valid? ::node %)]}
  (let [train-data (map (partial update-key key-of-objective-variable :Classes) train-data)
        node (create-node-from-data train-data)
        splitter (get-maximum-information-gain-splitter node)]
    (split-node node (:threshold splitter) (:feature splitter) max-depth)))

(defn predict
  "Predict objective-variable used by decision-tree
  `tree` is decision-tree made by `make-decision-tree` function.
  `data` is just one data like a
  Example of `data`:
    {:Sepal.Length 6.2, :Sepal.Width 3.4, :Petal.Length 5.4, :Petal.Width 2.3, :Species \"virginica\"}"
  [tree data]
  (cond (not (nil? (:predict tree)))
        (:predict tree)
        (> ((:feature tree) data) (:threshold tree)) (predict (:left tree) data)
        :else (predict (:right tree) data)))
