(ns propaganda.intervals-test
  (:use clojure.test)
  (:require [propaganda.stm :as p]
            [propaganda.intervals.common :as i]
            [propaganda.intervals.stm :as ii]
            [propaganda.support-values :as sv]
            [propaganda.values :as v]))

;; building height example

(defn fall-duration
  "Creates propagator from fall duration t to building height h with
  some uncertainty on the gravitational acceleration."
  [t h]
  (p/compound-propagator
   [t]
   (fn []
     (let [g          (p/make-cell)
           one-half   (p/make-cell)
           t-squared  (p/make-cell)
           gt-squared (p/make-cell)]
       ((p/constant (i/make-interval 9.789 9.832)) g)
       ((p/constant 0.5) one-half)
       (ii/quadratic t t-squared)
       (ii/product g t-squared gt-squared)
       (ii/product one-half gt-squared h)))))

(defn similar-triangles
  [s-ba h-ba s h]
  (p/compound-propagator
   [s-ba h-ba s]
   (fn []
     (let [ratio (p/make-cell)]
       (ii/product s-ba ratio h-ba)
       (ii/product s ratio h)))))

(deftest building-height-test
  (let [custom-merge (doto (v/default-merge) ii/extend-merge)]
    (binding [p/*merge* custom-merge]
     (let [building-height  (p/make-cell)
           fall-time        (p/make-cell)
           barometer-height (p/make-cell)
           barometer-shadow (p/make-cell)
           building-shadow  (p/make-cell)]

       (fall-duration fall-time building-height)
       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)

       (p/add-value fall-time (i/make-interval 2.9 3.1))
       (p/add-value building-shadow (i/make-interval 54.9 55.1))
       (p/add-value barometer-height (i/make-interval 0.3 0.32))
       (p/add-value barometer-shadow (i/make-interval 0.36 0.37))
       (p/add-value building-height 45.0)

       (is (= 45.0
              (p/get-value building-height)))
       (is (= (i/make-interval 54.9 55.1)
              (p/get-value building-shadow)))
       (is (= (i/make-interval 0.3 0.30327868852459017)
              (p/get-value barometer-height)))
       (is (= (i/make-interval 0.366 0.37)
              (p/get-value barometer-shadow)))
       (is (= (i/make-interval  3.025522031629098 3.0321598338046556)
              (p/get-value fall-time)))))))

;; building height with supported-values example

(deftest building-height-with-supported-values-test
  (let [custom-merge (doto (v/default-merge)
                       ii/extend-merge
                       sv/extend-merge)
        custom-contradictory? (doto (v/default-contradictory?)
                                sv/extend-contradictory?)]
    (binding [p/*merge* custom-merge]
     (let [building-height  (p/make-cell)
           fall-time        (p/make-cell)
           barometer-height (p/make-cell)
           barometer-shadow (p/make-cell)
           building-shadow  (p/make-cell)]

       (fall-duration fall-time building-height)
       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)

       (p/add-value building-shadow
                      (sv/supported
                       (i/make-interval 54.9 55.1)
                       :shadows))
       (p/add-value barometer-height
                      (sv/supported
                       (i/make-interval 0.3 0.32)
                       :shadows))
       (p/add-value barometer-shadow
                      (sv/supported
                       (i/make-interval 0.36 0.37)
                       :shadows))
       (p/add-value fall-time
                    (sv/supported
                     (i/make-interval 2.9 3.3)
                     :lousy-fall-time))
       (p/add-value fall-time
                      (sv/supported
                       (i/make-interval 2.9 3.1)
                       :better-fall-time))
       (p/add-value building-height
                      (sv/supported
                       45.0
                       :superintendent))

       (are [c v s] (and (is (= v (:value (p/get-value c))))
                         (is (= s (:support-set (p/get-value c)))))

            building-height
            45.0
            #{:superintendent}

            building-shadow
            (i/make-interval 54.9 55.1)
            #{:superintendent :shadows}

            barometer-height
            (i/make-interval 0.3 0.30327868852459017)
            #{:superintendent :shadows :better-fall-time}

            barometer-shadow
            (i/make-interval 0.366 0.37)
            #{:superintendent :shadows :better-fall-time}

            fall-time
            (i/make-interval  3.025522031629098 3.0321598338046556)
            #{:superintendent})))))
