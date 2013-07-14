(ns propaganda.intervals-test
  (:use clojure.test)
  (:require [propaganda.core :as p]
            [propaganda.intervals :as i]
            [propaganda.support-values :as sv]))

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
       (i/quadratic t t-squared)
       (i/product g t-squared gt-squared)
       (i/product one-half gt-squared h)))))

(defn similar-triangles
  [s-ba h-ba s h]
  (p/compound-propagator
   [s-ba h-ba s]
   (fn []
     (let [ratio (p/make-cell)]
       (i/product s-ba ratio h-ba)
       (i/product s ratio h)))))

(deftest building-height-test
  (let [custom-merge (doto (p/default-merge) i/extend-merge)]
    (binding [p/*merge* custom-merge]
     (let [building-height  (p/make-cell)
           fall-time        (p/make-cell)
           barometer-height (p/make-cell)
           barometer-shadow (p/make-cell)
           building-shadow  (p/make-cell)]

       (fall-duration fall-time building-height)
       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)

       (p/add-content fall-time (i/make-interval 2.9 3.1))
       (p/add-content building-shadow (i/make-interval 54.9 55.1))
       (p/add-content barometer-height (i/make-interval 0.3 0.32))
       (p/add-content barometer-shadow (i/make-interval 0.36 0.37))
       (p/add-content building-height 45.0)

       (is (= 45.0
              (p/get-content building-height)))
       (is (= (i/make-interval 54.9 55.1)
              (p/get-content building-shadow)))
       (is (= (i/make-interval 0.3 0.30327868852459017)
              (p/get-content barometer-height)))
       (is (= (i/make-interval 0.366 0.37)
              (p/get-content barometer-shadow)))
       (is (= (i/make-interval  3.025522031629098 3.0321598338046556)
              (p/get-content fall-time)))))))

;; building height with supported-values example

(deftest building-height-with-supported-values-test
  (let [custom-merge (doto (p/default-merge)
                       i/extend-merge
                       sv/extend-merge)
        custom-contradictory? (doto (p/default-contradictory?)
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

       (p/add-content building-shadow
                      (sv/supported
                       (i/make-interval 54.9 55.1)
                       :shadows))
       (p/add-content barometer-height
                      (sv/supported
                       (i/make-interval 0.3 0.32)
                       :shadows))
       (p/add-content barometer-shadow
                      (sv/supported
                       (i/make-interval 0.36 0.37)
                       :shadows))
       (p/add-content fall-time
                    (sv/supported
                     (i/make-interval 2.9 3.3)
                     :lousy-fall-time))
       (p/add-content fall-time
                      (sv/supported
                       (i/make-interval 2.9 3.1)
                       :better-fall-time))
       (p/add-content building-height
                      (sv/supported
                       45.0
                       :superintendent))

       (are [c v s] (and (is (= v (:value (p/get-content c))))
                         (is (= s (:support-set (p/get-content c)))))

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
