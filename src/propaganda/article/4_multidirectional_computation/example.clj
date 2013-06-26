(ns propaganda.article.4-multidirectional-computation)

;; Interval arithmetic

(defrecord Interval
    [lo hi])

(defn make-interval
  [lo hi]
  (Interval. lo hi))

(defn mul-interval
  "Multiplies the intervals. Assumes all limits are positive."
  [x y]
  (make-interval (* (:lo x) (:lo y)) (* (:hi x) (:hi y))))

(defn div-interval
  "Divides the intervals. Assumes all limits are strictly positive."
  [x y]
  (mul-interval x (make-interval (/ 1 (:hi y)) (/ 1 (:lo y)))))

(defn square-interval
  [x]
  (make-interval (* (:lo x) (:lo x))
                 (* (:hi x) (:hi x))))

(defn sqrt-interval
  [x]
  (make-interval (Math/sqrt (double (:lo x)))
                 (Math/sqrt (double (:hi x)))))

(defn empty-interval?
  [x]
  (> (:lo x) (:hi x)))

(defn intersect-intervals
  [x y]
  (make-interval
   (max (:lo x) (:lo y))
   (min (:hi x) (:hi y))))

;; Propagator framework

(def nothing (Object.))

(defn nothing?
  [thing]
  (= nothing thing))

(defn alert-propagators
  "Simple implementation of alerting propagators. The original article
  seems to indicate that scheduling should be used instead of just
  aggressively executing the propagators."
  [propagators]
  (doseq [propagator propagators]
    (propagator)))

(defprotocol Cell
  (new-neighbour! [this new-neighbour])
  (add-content    [this increment])
  (get-content    [this]))

(defn make-cell
  []
  (let [neighbours (ref nil)
        content    (ref nothing)]
    (reify
      Cell
      (new-neighbour!
        [this new-neighbour]
        (dosync
         (when (not (contains? (set @neighbours) new-neighbour))
           (alter neighbours conj new-neighbour)
           (alert-propagators [new-neighbour]))))
      (add-content
        [this increment]
        (dosync
         (cond
          (nothing? increment)   :ok
          (nothing? @content)    (do (ref-set content increment)
                                     (alert-propagators @neighbours))
          :else                  (let [new-range (intersect-intervals
                                                  @content increment)]
                                   (cond
                                    (= new-range @content)
                                    :ok

                                    (empty-interval? new-range)
                                    (throw (Exception. "inconsistency"))

                                    :else
                                    (do
                                      (ref-set content new-range)
                                      (alert-propagators @neighbours)))))))
      (get-content
        [this]
        @content))))

(defn propagator
  "Adds a new propagator (to-do) to the neighbours and guarantees that
  it is called (although adding it should have that side-effect, but not
  doing it causes a failure - there is something I haven't thought
  through)"
  [neighbours to-do]
  (doseq [cell neighbours]
    (new-neighbour! cell to-do))
  (alert-propagators [to-do]))

(defn lift-to-cell-contents
  "Returns a safe-guarded version of f which ensures that all arguments
  are different than nothing."
  [f]
  (fn [& args]
    (if (some nothing? args)
      nothing
      (apply f args))))

(defn function->propagator-constructor
  "Returns a propagtor constructor which will lift the content of f
  applied to the first cells to the last cell."
  [f]
  (fn [& cells]
    (let [inputs (butlast cells)
          output (last cells)
          lifted-f (lift-to-cell-contents f)]
      (propagator
       inputs
       (fn [] (add-content
              output
              (apply lifted-f (map get-content inputs))))))))

(defn compound-propagator
  "Constructs a propagtor which will observe the neighbours cells and
  run to-build when their values are all different from nothing."
  [neighbours to-build]
  (let [done? (ref false)
        test (fn [] (when-not @done?
                     (when-not (some nothing? (map get-content neighbours))
                       (ref-set done? true)
                       (to-build))))]
    (propagator neighbours test)))

;; Propagator constructors

(def multiplier (function->propagator-constructor mul-interval))
(def divider    (function->propagator-constructor div-interval))
(def squarer    (function->propagator-constructor square-interval))
(def sqrter     (function->propagator-constructor sqrt-interval))

(defn constant
  [value]
  (function->propagator-constructor (fn [] value)))

;; Multidirectional propagators constructors (relations)

(defn product
  "Creates the product relation x * y = total between the cells."
  [x y total]
  (multiplier x y total)
  (divider total x y)
  (divider total y x))

(defn quadratic
  "Creates the quadratic relation x * x = x-squared between the cells."
  [x x-squared]
  (squarer x x-squared)
  (sqrter x-squared x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Building height

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
       ((constant (make-interval 0.5 0.5)) one-half)
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


;; Trying it all out
(clojure.pprint/pprint
  (let [building-height (make-cell)

        fall-time       (make-cell)

        barometer-height (make-cell)
        barometer-shadow (make-cell)
        building-shadow  (make-cell)]

    (fall-duration fall-time building-height)
    (add-content fall-time (make-interval 2.9 3.1))

    (similar-triangles barometer-shadow barometer-height
                       building-shadow building-height)
    (add-content building-shadow (make-interval 54.9 55.1))
    (add-content barometer-height (make-interval 0.3 0.32))
    (add-content barometer-shadow (make-interval 0.36 0.37))

    [(get-content building-height)
     (get-content barometer-height)
     (get-content fall-time)]))
;; => [{:lo 44.51351351351351, :hi 47.24276000000001}
;;     {:lo 0.3, :hi 0.3183938287795994}
;;     {:lo 3.0091234174691017, :hi 3.1}]


;; Bribing the building's superintendent
(clojure.pprint/pprint
  (let [building-height (make-cell)

        fall-time       (make-cell)

        barometer-height (make-cell)
        barometer-shadow (make-cell)
        building-shadow  (make-cell)]

    (fall-duration fall-time building-height)
    (add-content fall-time (make-interval 2.9 3.1))

    (similar-triangles barometer-shadow barometer-height
                       building-shadow building-height)
    (add-content building-shadow (make-interval 54.9 55.1))
    (add-content barometer-height (make-interval 0.3 0.32))
    (add-content barometer-shadow (make-interval 0.36 0.37))

    (add-content building-height (make-interval 45 45))

    [(get-content building-height)
     (get-content building-shadow)
     (get-content barometer-height)
     (get-content barometer-shadow)
     (get-content fall-time)]))
;; => [{:lo 45.0, :hi 45.0}
;;     {:lo 54.9, :hi 55.1}
;;     {:lo 0.3, :hi 0.30327868852459017}
;;     {:lo 0.366, :hi 0.37}
;;     {:lo 3.025522031629098, :hi 3.0321598338046556}]
