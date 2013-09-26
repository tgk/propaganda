(in-ns 'user)

(use 'propaganda.stm)
(use 'propaganda.values)
(use '[propaganda.intervals.common :exclude [extend-merge]])
(use 'propaganda.intervals.stm)

(defn fall-duration
  "Creates propagator from fall duration t to building height h with
  some uncertainty on the gravitational acceleration."
  [t h]
  (compound-propagator
   [t]
   (fn []
     (let [g          (make-cell)
           one-half   (make-cell)
           t-squared  (make-cell)
           gt-squared (make-cell)]
       ((constant (make-interval 9.789 9.832)) g)
       ((constant 0.5) one-half)
       (quadratic t t-squared)
       (product g t-squared gt-squared)
       (product one-half gt-squared h)))))

(defn similar-triangles
  [s-ba h-ba s h]
  (compound-propagator
   [s-ba h-ba s]
   (fn []
     (let [ratio (make-cell)]
       (product s-ba ratio h-ba)
       (product s ratio h)))))

;; Trying it all out while bribing the building's superintendent
(clojure.pprint/pprint
 (let [custom-merge (doto (default-merge) extend-merge)]
   (binding [*merge* custom-merge]
     (let [building-height (make-cell)

           fall-time       (make-cell)

           barometer-height (make-cell)
           barometer-shadow (make-cell)
           building-shadow  (make-cell)]

       (fall-duration fall-time building-height)
       (add-value fall-time (make-interval 2.9 3.1))

       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)
       (add-value building-shadow (make-interval 54.9 55.1))
       (add-value barometer-height (make-interval 0.3 0.32))
       (add-value barometer-shadow (make-interval 0.36 0.37))

       (add-value building-height 45.0)

       [(get-value building-height)
        (get-value building-shadow)
        (get-value barometer-height)
        (get-value barometer-shadow)
        (get-value fall-time)]))))
;; => [45
;;     {:lo 54.9, :hi 55.1}
;;     {:lo 0.3, :hi 0.30327868852459017}
;;     {:lo 0.366, :hi 0.37}
;;     {:lo 3.025522031629098, :hi 3.0321598338046556}]
