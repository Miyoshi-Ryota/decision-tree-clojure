(ns decision-tree.core-test
  (:require [clojure.test :refer :all]
            [decision-tree.core :refer :all]))

(deftest test-update-key
  (testing "expectation use case of the function"
    (let [input-data {:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Species "setosa"}
          expected-output-data {:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Classes "setosa"}]
      (is (= expected-output-data
             (#'decision-tree.core/update-key :Species :Classes input-data))))))

(deftest test-gini-impurity
  (testing "expectation use case of the function"
    (let [data-expected-zero '("a" "a" "a")
          data-expected-two-thirds '("a" "b" "c")]
      (is (= 0.0 (#'decision-tree.core/gini-impurity data-expected-zero)))
      (is (= (double (/ 2 3)) (#'decision-tree.core/gini-impurity data-expected-two-thirds))))))
