(ns propaganda.system-intervals-test
  (:use clojure.test)
  (:require [propaganda.system :as p :refer [add-value get-value]]
            [propaganda.intervals.common :as i :refer [make-interval]]
            [propaganda.values :as values]
            [propaganda.support-values :as sv :refer [supported]]
            [propaganda.intervals.system :refer [quadratic product]]))

(defn fall-duration [system time height]
  (let [[g one-half t-squared gt-squared] (repeatedly gensym)]
    (-> system
        ((p/constant (make-interval 9.789 9.832)) g)
        ((p/constant 0.5) one-half)
        (quadratic time t-squared)
        (product g t-squared gt-squared)
        (product one-half gt-squared height))))

(defn similar-triangles [system & shadow-height-pairs]
  (let [ratio (gensym)]
    (reduce (fn [system [s h]] (product system s ratio h))
            system
            (partition 2 shadow-height-pairs))))

(deftest system-building-height-test
  (let [system
        (-> (p/make-system (i/extend-merge (values/default-merge)) (values/default-contradictory?))
            (fall-duration :fall-time :building-height)
            (similar-triangles :barometer-shadow :barometer-height
                               :building-shadow :building-height)
            (add-value :fall-time (make-interval 2.9 3.1))
            (add-value :building-shadow (make-interval 54.9 55.1))
            (add-value :barometer-height (make-interval 0.3 0.32))
            (add-value :barometer-shadow (make-interval 0.36 0.37))
            (add-value :building-height 45.0))]
    (is (= 45.0 (get-value system :building-height)))
    (is (= (make-interval 54.9 55.1) (get-value system :building-shadow)))
    (is (= (make-interval 0.3 0.30327868852459017) (get-value system :barometer-height)))
    (is (= (make-interval 0.366 0.37) (get-value system :barometer-shadow)))
    (is (= (make-interval 3.025522031629098 3.0321598338046556) (get-value system :fall-time)))))

(deftest system-building-height-with-support-test
  (let [system
        (-> (p/make-system (doto (values/default-merge) i/extend-merge sv/extend-merge)
                           (doto (values/default-contradictory?) sv/extend-contradictory?))
            (fall-duration :fall-time :building-height)
            (similar-triangles :barometer-shadow :barometer-height
                               :building-shadow :building-height)
            (add-value :fall-time (supported (make-interval 2.9 3.3) :lousy-fall-time))
            (add-value :building-shadow (supported (make-interval 54.9 55.1) :shadows))
            (add-value :barometer-height (supported (make-interval 0.3 0.32) :shadows))
            (add-value :barometer-shadow (supported (make-interval 0.36 0.37) :shadows))
            (add-value :fall-time (supported (make-interval 2.9 3.1) :better-fall-time))
            (add-value :building-height (supported 45.0 :superintendent)))]
    (are [c v s] (and (is (= v (:value (get-value system c))))
                      (is (= s (:support-set (get-value system c)))))

         :building-height
         45.0
         #{:superintendent}

         :building-shadow
         (i/make-interval 54.9 55.1)
         #{:superintendent :shadows}

         :barometer-height
         (i/make-interval 0.3 0.30327868852459017)
         #{:superintendent :shadows :better-fall-time}

         :barometer-shadow
         (i/make-interval 0.366 0.37)
         #{:superintendent :shadows :better-fall-time}

         :fall-time
         (i/make-interval  3.025522031629098 3.0321598338046556)
         #{:superintendent})))

