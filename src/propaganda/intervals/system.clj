(ns propaganda.intervals.system
  (:require [propaganda.system :as propaganda]
            [propaganda.intervals.common :as intervals]))

;; Propagator constructors

(def multiplier (propaganda/function->propagator-constructor intervals/generic-mul))
(def divider    (propaganda/function->propagator-constructor intervals/generic-div))
(def squarer    (propaganda/function->propagator-constructor intervals/generic-square))
(def sqrter     (propaganda/function->propagator-constructor intervals/generic-sqrt))

;; Multidirectional propagators constructors (relations)

(defn product
  "Creates the product relation x * y = total between the cells."
  [system x y total]
  (-> system
      (multiplier x y total)
      (divider total x y)
      (divider total y x)))

(defn quadratic
  "Creates the quadratic relation x * x = x-squared between the cells."
  [system x x-squared]
  (-> system
      (squarer x x-squared)
      (sqrter x-squared x)))

(def extend-merge
  "Extends the merge operator with support for interval under the system
  version of propagators."
  intervals/extend-merge)
