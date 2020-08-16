(ns decision-tree.core
  (:require [incanter.datasets])
  (:require [clojure.spec.alpha :as s]))

(def test-data (random-sample 0.3 iris))
(def train-data (filter #(not (.contains test-data %1)) iris))
(def root {:feature nil :threshold nil :data train-data :right nil :left nil}) ; thresholdは以上ならleftに以下ならrightに進む．

(defn gini-impurity [y]
  (let [number-of-each-data (map #(count (val %)) (group-by identity y))]
    (- 1 (apply + (map #(Math/pow %1 2) (map #(/ % (apply + number-of-each-data)) number-of-each-data))))))

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
