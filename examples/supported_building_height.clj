(in-ns 'user)

(use 'propaganda.stm)
(use 'propaganda.values)
(use '[propaganda.intervals.common :exclude [extend-merge]])
(use 'propaganda.intervals.stm)
(require '(propaganda [support-values :as support-values]))

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
 (let [custom-merge (doto (default-merge)
                      extend-merge
                      support-values/extend-merge)
       custom-contradictory? (doto (default-contradictory?)
                               support-values/extend-contradictory?)]
   (binding [*merge* custom-merge
             ;; No contradictions are found, but including it anyways
             *contradictory?* custom-contradictory?]
     (let [building-height (make-cell)

           fall-time       (make-cell)

           barometer-height (make-cell)
           barometer-shadow (make-cell)
           building-shadow  (make-cell)]

       (fall-duration fall-time building-height)
       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)


       (add-value building-shadow
                    (support-values/supported
                     (make-interval 54.9 55.1)
                     :shadows))
       (add-value barometer-height
                    (support-values/supported
                     (make-interval 0.3 0.32)
                     :shadows))
       (add-value barometer-shadow
                    (support-values/supported
                     (make-interval 0.36 0.37)
                     :shadows))


       (add-value fall-time
                    (support-values/supported
                     (make-interval 2.9 3.3)
                     :lousy-fall-time))


       (add-value fall-time
                    (support-values/supported
                     (make-interval 2.9 3.1)
                     :better-fall-time))


       (add-value building-height
                    (support-values/supported
                     45.0
                     :superintendent))

       [(get-value building-height)
        (get-value building-shadow)
        (get-value barometer-height)
        (get-value barometer-shadow)
        (get-value fall-time)]))))
;; [{:value 45.0, :support-set #{:superintendent}}
;; {:value {:lo 54.9, :hi 55.1},
;;  :support-set #{:superintendent :shadows}}
;; {:value {:lo 0.3, :hi 0.30327868852459017},
;;  :support-set #{:better-fall-time :superintendent :shadows}}
;; {:value {:lo 0.366, :hi 0.37},
;;  :support-set #{:better-fall-time :superintendent :shadows}}
;; {:value {:lo 3.025522031629098, :hi 3.0321598338046556},
;;  :support-set #{:superintendent}}]
;;
;; Play around what information is available to change the results
