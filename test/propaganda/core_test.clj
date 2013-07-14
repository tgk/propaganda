(ns propaganda.core-test
  (:use clojure.test)
  (:require [propaganda.core :as p]))

(deftest simple-cells-test

  (testing "Can chage the value of a cell"
    (let [c (p/make-cell)]
      (is (p/nothing? (p/get-content c)))
      (binding [p/*merge* (p/default-merge)]
        (p/add-content c :foo)
        (is (not (p/nothing? (p/get-content c))))
        (is (= :foo (p/get-content c))))))

  (testing "Operating with two cells is safe"
    (let [c (p/make-cell)
          d (p/make-cell)]
      (is (p/nothing? (p/get-content c)))
      (is (p/nothing? (p/get-content d)))
      (binding [p/*merge* (p/default-merge)]
        (p/add-content c :foo)
        (is (not (p/nothing? (p/get-content c))))
        (is (= :foo (p/get-content c)))
        (is (p/nothing? (p/get-content d))))))

  (testing "Merging nothing with nothing yields nothing"
    (let [c (p/make-cell)]
      (binding [p/*merge* (p/default-merge)]
        (p/add-content c p/nothing))
      (is (p/nothing? (p/get-content c))))))

(deftest default-merge-fails-on-contradictions

  (testing "Changing value to same value works"
    (let [c (p/make-cell)]
      (binding [p/*merge* (p/default-merge)]
        (p/add-content c :foo)
        (p/add-content c :foo))
      (is (= :foo (p/get-content c)))))

  (testing "Changing value to something new throws an exception"
    (try
      (let [c (p/make-cell)]
        (binding [p/*merge* (p/default-merge)]
          (p/add-content c :foo)
          (p/add-content c :bar)))
      (assert false "Should have thrown exception")
      (catch Throwable t
             (when-not (.startsWith (.getMessage t) "Inconsistency:")
               (assert
                false
                (str "Did not throw correct type of assertion, threw: "
                     t)))))))

(def squarer
  (p/function->propagator-constructor
   (fn [val] (* val val))))

(deftest test-propagators
  (testing "y is the square of x"
    (let [x (p/make-cell)
          y (p/make-cell)]
      (binding [p/*merge* (p/default-merge)]
        (squarer x y)
        (is (p/nothing? (p/get-content x)))
        (is (p/nothing? (p/get-content y)))
        (p/add-content x 10.0)
        (is (= 10.0 (p/get-content x)))
        (is (= 100.0 (p/get-content y)))))))

(def sqrter
  (p/function->propagator-constructor
   (fn [val] (Math/sqrt val))))

(defn quadratic
  [x x-squared]
  (squarer x x-squared)
  (sqrter  x-squared x))

(deftest test-two-way-propagators
  (testing "y is the square of x"
    (let [x (p/make-cell)
          y (p/make-cell)]
      (binding [p/*merge* (p/default-merge)]
        (quadratic x y)
        (is (p/nothing? (p/get-content x)))
        (is (p/nothing? (p/get-content y)))
        (p/add-content x 10.0)
        (is (= 10.0 (p/get-content x)))
        (is (= 100.0 (p/get-content y))))))
  (testing "x is the square-root of y"
    (let [x (p/make-cell)
          y (p/make-cell)]
      (binding [p/*merge* (p/default-merge)]
        (quadratic x y)
        (is (p/nothing? (p/get-content x)))
        (is (p/nothing? (p/get-content y)))
        (p/add-content y 100.0)
        (is (= 100.0 (p/get-content y)))
        (is (= 10.0 (p/get-content x)))))))
