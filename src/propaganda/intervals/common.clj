(ns propaganda.intervals.common
  (:require [propaganda.values :as values]
            [propaganda.generic-operators :as go]
            [propaganda.support-values :as support-values]))

;; Interval arithmetics

(defrecord Interval
    [lo hi])

(defn make-interval
  "Returns a new closed interval from lo to hi."
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
  "Determines if the interval is empty."
  [x]
  (> (:lo x) (:hi x)))

(defn intersect-intervals
  "Creates an intersection of the intervals."
  [x y]
  (make-interval
   (max (:lo x) (:lo y))
   (min (:hi x) (:hi y))))

(defn interval?
  "Returns true iff x is an interval."
  [x]
  (isa? (type x) Interval))

(defn ensure-inside
  "If number is in interval, the number is returned. If not, a
  descriptive contradiction is returned."
  [interval number]
  (if (<= (:lo interval) number (:hi interval))
    number
    (values/contradiction
     (str number " not in interval [" (:lo interval?) ", " (:hi interval) "]"))))

;; Generic standard arithmetic operations

(defn ->interval
  "Ensures x is an interval. If x is already an interval, x is
  returned. If x is not, an interval from x to x is returned."
  [x]
  (if (interval? x)
    x
    (make-interval x x)))

(defn coercing
  "Returns a version of f that will coerce arguments using coercer
  before applying them."
  [coercer f]
  (fn [& args]
    (apply f (map coercer args))))

(def generic-mul (doto (go/generic-operator *)
                   (go/assign-operation mul-interval
                                        interval? interval?)
                   (go/assign-operation (coercing ->interval mul-interval)
                                        number? interval?)
                   (go/assign-operation (coercing ->interval mul-interval)
                                        interval? number?)))
(def generic-div (doto (go/generic-operator /)
                   (go/assign-operation div-interval
                                        interval? interval?)
                   (go/assign-operation (coercing ->interval div-interval)
                                        number? interval?)
                   (go/assign-operation (coercing ->interval div-interval)
                                        interval? number?)))
(def generic-square (doto (go/generic-operator (fn [x] (* x x)))
                      (go/assign-operation
                       square-interval
                       interval?)))
(def generic-sqrt (doto (go/generic-operator (fn [x] (Math/sqrt (double x))))
                    (go/assign-operation
                     sqrt-interval
                     interval?)))

;; Supported values

(defn flat?
  "Determines if thing is flat, i.e. an interval or a number."
  [thing]
  (or (interval? thing)
      (number? thing)))

(doseq [generic-op [generic-mul generic-div]]
  ;; supported values support
  (go/assign-operation generic-op
                       (support-values/supported-unpacking generic-op)
                       support-values/supported? support-values/supported?)
  (go/assign-operation generic-op
                       (coercing support-values/->supported generic-op)
                       support-values/supported? flat?)
  (go/assign-operation generic-op
                       (coercing support-values/->supported generic-op)
                       flat? support-values/supported?))

(doseq [generic-op [generic-square generic-sqrt]]
  (go/assign-operation generic-op
                       (support-values/supported-unpacking generic-op)
                       support-values/supported?))

;; Extend supplied merge

(defn extend-merge
  "Extends the supplied generic operator with interval mergings. These
  also include merging numbers with intervals."
  [generic-merge-operator]
  (doto generic-merge-operator
    (go/assign-operation (fn [content increment]
                           (let [new-range (intersect-intervals content increment)]
                             (if (empty-interval? new-range)
                               (values/contradiction
                                (str "Non-overlapping intervals: " content " and " increment))
                               new-range)))
                         interval? interval?)
    (go/assign-operation (fn [content increment]
                           (ensure-inside increment content))
                         number? interval?)
    (go/assign-operation (fn [content increment]
                           (ensure-inside content increment))
                         interval? number?)
    ;; support values merging
    (go/assign-operation (coercing support-values/->supported generic-merge-operator)
                         support-values/supported? flat?)
    (go/assign-operation (coercing support-values/->supported generic-merge-operator)
                         flat? support-values/supported?)))
