# decision-tree

A Clojure library designed to run decision-tree. 
The library's decision-tree targets classification problem.

## Usage
* Add :dependencies in your project.clj file.
```
:dependencies [[org.clojure/clojure "1.10.1"]
               [mrcsce/decision-tree "0.1.0"]]
```

* Add :require in your clj file to import the library.
```
(:require [decision-tree.core :as dt])
```

* Sample code (Note: If you want to run this sample code, you also add `[incanter "1.5.5"]` in your dependencies.)
```clojure
(ns your-project.core
  (:require [decision-tree.core :as dt])
  (:require [incanter.datasets]))

;; To indicate sample code, we use iris dataset.
(def iris (->> (incanter.datasets/get-dataset :iris)
               :rows))

;; iris dataset is like a following data structure.
(take 3 iris)
;; =>({:Sepal.Length 5.1, :Sepal.Width 3.5, :Petal.Length 1.4, :Petal.Width 0.2, :Species \"setosa\"}
;     {:Sepal.Length 4.9, :Sepal.Width 3.0, :Petal.Length 1.4, :Petal.Width 0.2, :Species \"setosa\"}
;;    {:Sepal.Length 4.7, :Sepal.Width 3.2, :Petal.Length 1.3, :Petal.Width 0.2, :Species \"setosa\"})

;; split iris-dataset to test-data and train-data.
(def test-data (random-sample 0.3 iris))
(def train-data (filter #(not (.contains test-data %1)) iris))

;; max-depth-of-decision-tree is parameter of decision-tree.
;; If you set too big value to this, more likely overfitting.
;; If you set too small value to this, more likely underfitting.
(def max-depth-of-decision-tree 3)

;; A signature of dt/make-decision-tree is `make-decision-tree [train-data max-depth key-of-objective-variable]`.
;; `train-data` is coll of map(data).
;; `max-depth` is positive integer.
;; `key-of-objective-variable` is keyword which we want to predict for example it is :Species in iris dataset.
(def decision-tree (dt/make-decision-tree train-data max-depth-of-decision-tree :Species))

;; Example of predict function in this library.
;; The predict function take just one data like a
;; {:Sepal.Length 5.9, :Sepal.Width 3.0, :Petal.Length 5.1, :Petal.Width 1.8, :Species \"virginica\"})
;; or {:Sepal.Length 5.9, :Sepal.Width 3.0, :Petal.Length 5.1, :Petal.Width 1.8}.
(dt/predict decision-tree (first test-data))
;; => \"setosa\"

;; the predict is correct.
(first test-data)
;; => {:Sepal.Length 5.0, :Sepal.Width 3.4, :Petal.Length 1.5, :Petal.Width 0.2, :Species \"setosa\"}\n


;; Example of correct-rate in all test-data.
(def results
  (map #(= %1 %2)
       (->> test-data
            (map (partial dt/predict decision-tree)))
       (->> test-data
            (map :Species))))

(defn calculate-correct-rate
  [results]
  (let [results (frequencies results)
        k (->> (keys results)
               (map str)
               (map keyword))
        v (vals results)
        r (zipmap k v)
        number-of-true (:true r)
        number-of-data (+ (:true r) (:false r))]
    (->> (/ number-of-true number-of-data)
         double)))

(calculate-correct-rate results)
;;=> 0.9761904761904762
;; Accomplish 97% accuracy in test data!!
```

## License

Copyright 2020 Ryota Miyoshi

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

