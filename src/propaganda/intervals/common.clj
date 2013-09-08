(ns propaganda.intervals.common
  (:require [propaganda.values :as values]
            [propaganda.generic-operators :as go]
            [propaganda.support-values :as support-values]))

;; Interval arithmetics

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

(defn interval?
  [x]
  (isa? (type x) Interval))

(defn ensure-inside
  [interval number]
  (if (<= (:lo interval) number (:hi interval))
    number
    (values/contradiction
     (str number " not in interval [" (:lo interval?) ", " (:hi interval) "]"))))

;; Generic standard arithmetic operations

(defn ->interval
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

(defn boolean?
  [thing]
  (or (= thing false) (= thing true)))

(defn flat?
  "Determines if thing is flat, i.e. an interval, a number or a
  boolean."
  [thing]
  (or (interval? thing)
      (number? thing)
      ;; TODO: We don't support booleans in the interval namespace yet
      #_(boolean? thing)))

;; TODO: Remember to extend this list when more operations are
;; implemented
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
