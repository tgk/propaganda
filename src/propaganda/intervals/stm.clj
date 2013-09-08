(ns propaganda.intervals.stm
  (:require [propaganda.stm :as propaganda]
            [propaganda.tms.stm :as tms]
            [propaganda.generic-operators :as go]
            [propaganda.intervals.common :as intervals]
            [propaganda.support-values :as support-values]))

;; Propagator constructors

(def multiplier (propaganda/function->propagator-constructor intervals/generic-mul))
(def divider    (propaganda/function->propagator-constructor intervals/generic-div))
(def squarer    (propaganda/function->propagator-constructor intervals/generic-square))
(def sqrter     (propaganda/function->propagator-constructor intervals/generic-sqrt))

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

;;

(doseq [generic-op [intervals/generic-mul intervals/generic-div]]
  ;; tms support
  (go/assign-operation generic-op
                       (tms/full-tms-unpacking generic-op)
                       tms/tms? tms/tms?)
  (go/assign-operation generic-op
                       (intervals/coercing tms/->tms generic-op)
                       tms/tms? support-values/supported?)
  (go/assign-operation generic-op
                       (intervals/coercing tms/->tms generic-op)
                       support-values/supported? tms/tms?)
  (go/assign-operation generic-op
                       (intervals/coercing tms/->tms generic-op)
                       tms/tms? intervals/flat?)
  (go/assign-operation generic-op
                       (intervals/coercing tms/->tms generic-op)
                       intervals/flat? tms/tms?))

(doseq [generic-op [intervals/generic-square intervals/generic-sqrt]]
  (go/assign-operation generic-op
                       (tms/full-tms-unpacking generic-op)
                       tms/tms?))

;; Extend supplied merge

(defn extend-merge
  "Extends the supplied generic operator with interval mergings. These
  also include merging numbers with intervals."
  [generic-merge-operator]
  (doto generic-merge-operator
    (intervals/extend-merge)
    ;; tms merging
    (go/assign-operation (intervals/coercing tms/->tms generic-merge-operator)
                         tms/tms? support-values/supported?)
    (go/assign-operation (intervals/coercing tms/->tms generic-merge-operator)
                         support-values/supported? tms/tms?)
    (go/assign-operation (intervals/coercing tms/->tms generic-merge-operator)
                         tms/tms? intervals/flat?)
    (go/assign-operation (intervals/coercing tms/->tms generic-merge-operator)
                         intervals/flat? tms/tms?)))
