(in-ns 'user)

(use 'propaganda.system)
(use 'propaganda.system-intervals)

(defn fall-duration
  "Creates propagator from fall duration t to building height h with
  some uncertainty on the gravitational acceleration."
  [system t h]
  (let [g          (gensym)
        one-half   (gensym)
        t-squared  (gensym)
        gt-squared (gensym)]
    (-> system
        ((constant (make-interval 9.789 9.790)) g)
        ((constant 0.5) one-half)
        (quadratic t t-squared)
        (product g t-squared gt-squared)
        (product one-half gt-squared h))))

(defn similar-triangles
  [system s-ba h-ba s h]
  (let [ratio (gensym)]
    (-> system
        (product s-ba ratio h-ba)
        (product s ratio h))))

;; Trying it all out while bribing the building's superintendent
(clojure.pprint/pprint
 (let [custom-merge (doto (default-merge) extend-merge)
       system (make-system custom-merge (default-contradictory?))
       result-system (-> system

                         (fall-duration :fall-time :building-height)
                         (add-value :fall-time (make-interval 2.9 3.1))

                         (similar-triangles :barometer-shadow :barometer-height
                                            :building-shadow :building-height)
                         (add-value :building-shadow (make-interval 54.9 55.1))
                         (add-value :barometer-height (make-interval 0.3 0.32))
                         (add-value :barometer-shadow (make-interval 0.36 0.37))

                         (add-value :building-height 45.0))]

   [(get-value result-system :building-height)
    (get-value result-system :building-shadow)
    (get-value result-system :barometer-height)
    (get-value result-system :barometer-shadow)
    (get-value result-system :fall-time)]))
;; => [45.0
;;     {:lo 54.9, :hi 55.1}
;;     {:lo 0.3, :hi 0.30327868852459017}
;;     {:lo 0.366, :hi 0.37}
;;     {:lo 3.032004969797108, :hi 3.0321598338046556}]
