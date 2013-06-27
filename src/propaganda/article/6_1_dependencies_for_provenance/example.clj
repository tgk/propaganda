(ns propaganda.article.6-1-dependencies-for-provenance)

;; Generic operations

(defn all-preds?
  "Returns true iff (pred vals) is truthy for all preds paired with
  vals."
  [preds vals]
  (and (= (count preds) (count vals))
       (every?
        identity
        (for [[pred val] (map vector preds vals)] (pred val)))))

(defn assign-operation
  "Takes a generic operator and returns a new genric operator where
  operation is invoked if preds are truthy for arguments passed to
  operator. If one or more predicates are not thruthy, the original
  generic-operator is tried. A plain function can be used as initial
  generic-operator."
  [generic-operator operation & preds]
  (fn [& args]
    (if (all-preds? preds args)
      (apply operation args)
      (apply generic-operator args))))

;; example of using genric operator
(comment
  (let [generic-op (-> (fn [n m] :default)
                       (assign-operation (fn [n m] [(inc n) (dec m)])
                                         even? odd?))]
    (generic-op 2 5)))
;; => [3 4]

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

(defn any?
  [thing]
  (not= nothing thing))

;; Insigth: This method works, but could potentially blow the stack. An
;; alternative strategy would be to have two refs: alerting-propagators
;; and propagator-queue. alert-propagators should add the propagators to
;; the queue, check to see if af consume loop is running, and if not,
;; start one
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

;; Supported datastructure and functions

(defrecord Supported
    [value support-set])

(defn supported
  [value support]
  (Supported. value (set [support])))

(defn more-informative-support?
  "Returns true if the support-set for support-2 contains strictly more
  information than the support-set in support-1."
  [support-1 support-2]
  (and (not= (:support-set support-1) (:support-set support-2))
       (clojure.set/subset? (:support-set support-1) (:support-set support-2))))

(defn merge-supports
  [& supports]
  (apply clojure.set/union (map :support-set supports)))

;; Problem: recursion will make this fail as the function won't be able
;; to call itself. Might have to rethink generic-operation and make it
;; influence var with meta data
(defn support-merger
  "Creates a new merge function with added support for supported values. Will revert to merge "
  [merge])

;; *merge* is introduced as the merging function
(defrecord Contradiction [])

(def the-contradiction (Contradiction.))

(defn contradictory?
  [x]
  (= x the-contradiction))

(def default-merge
  "The default merge function returns the content if the increment is
  nothing, and the increment if the content is nothing. This aligns with
  the cond behaviour in add-content from the previous sections.
  Otherwise just checks to see if the values are the same, the default
  merge function can be extended using assign-operation."
  (-> (fn [content increment]
        (if (= content increment)
          content
          (with-meta the-contradiction
            {:inconsistency-reason (str (pr-str content)
                                        " != "
                                        (pr-str increment))})))
      (assign-operation (fn [content increment] content)
                        any? nothing?)
      (assign-operation (fn [content increment] increment)
                        nothing? any?)))

(def ^:dynamic *merge*
  "The merge function used by the cells. Can be extended using
  assing-operation and bound with binding."
  default-merge)

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
         ;; we use *merge* instead
         (let [answer (*merge* @content increment)]
           (cond
            (= answer @content)     :ok
            (contradictory? answer) (throw (Exception.
                                            (str "Inconsistency: "
                                                 (:inconsistency-reason
                                                  (meta answer)))))
            :else                   (do
                                      (ref-set content answer)
                                      (alert-propagators @neighbours))))))
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

;; Custom merger that supports intervals and numbers

;; numbers against numbers are supported out of the box with the default
;; *merge*, interval support has to be added

(defn interval?
  [x]
  (isa? (class x) Interval))

(defn ensure-inside
  [interval number]
  (if (<= (:lo interval) number (:hi interval))
    number
    (with-meta
      the-contradiction
      {:inconsistency-reason
       (str number " not in interval ["
            (:lo interval?) ", " (:hi interval) "]")})))

(def custom-merge
  (-> default-merge
      (assign-operation (fn [content increment]
                          (let [new-range (intersect-intervals content increment)]
                            (if (empty-interval? new-range)
                              the-contradiction
                              new-range)))
                        interval? interval?)
      (assign-operation (fn [content increment]
                          (ensure-inside increment content))
                        number? interval?)
      (assign-operation (fn [content increment]
                          (ensure-inside content increment))
                        interval? number?)))

;; Generic standard arithmetic operations

(defn ->interval
  [x]
  (if (interval? x)
    x
    (make-interval x x)))

(defn coercing
  [coercer f]
  (fn [& args]
    (apply f (map coercer args))))

(def generic-mul (-> *
                     (assign-operation mul-interval
                                       interval? interval?)
                     (assign-operation (coercing ->interval mul-interval)
                                       number? interval?)
                     (assign-operation (coercing ->interval mul-interval)
                                       interval? number?)))
(def generic-div (-> /
                     (assign-operation div-interval
                                       interval? interval?)
                     (assign-operation (coercing ->interval div-interval)
                                       number? interval?)
                     (assign-operation (coercing ->interval div-interval)
                                       interval? number?)))
(def generic-square (assign-operation (fn [x] (* x x))
                                      square-interval
                                      interval?))
(def generic-sqrt (assign-operation (fn [x] (Math/sqrt (double x)))
                                    sqrt-interval
                                    interval?))

;; Propagator constructors

(def multiplier (function->propagator-constructor generic-mul))
(def divider    (function->propagator-constructor generic-div))
(def squarer    (function->propagator-constructor generic-square))
(def sqrter     (function->propagator-constructor generic-sqrt))

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
 (binding [*merge* custom-merge]
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

     (add-content building-height 45.0)

     [(get-content building-height)
      (get-content building-shadow)
      (get-content barometer-height)
      (get-content barometer-shadow)
      (get-content fall-time)])))
;; => [45
;;     {:lo 54.9, :hi 55.1}
;;     {:lo 0.3, :hi 0.30327868852459017}
;;     {:lo 0.366, :hi 0.37}
;;     {:lo 3.025522031629098, :hi 3.0321598338046556}]
