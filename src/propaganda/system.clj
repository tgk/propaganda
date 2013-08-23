(ns propaganda.system
  (:require [propaganda.generic-operators :as generic-operators]))

(defrecord PropagatorSystem
    [values propagators merge contradictory? alert-queue])

(defprotocol PropagatorSystemProtocol
  (make-cell      [this])
  (add-value      [this cell value])
  (get-value      [this cell])
  (add-propagator [this cells f])
  (cool           [this])
  (stabile?       [this])

  (-alert-propagators     [this fs])
  (-alert-all-propagators [this]))

;;; START: Stuff that is independent of propagator system implementation strategy

(def nothing (Object.))

(defn nothing?
  [thing]
  (= nothing thing))

(defn any?
  [thing]
  (not= nothing thing))





(defrecord Contradiction [reason])

(defn contradiction
  [reason]
  (Contradiction. reason))

(defn- base-contradictory?
  [x]
  (isa? (class x) Contradiction))

(defn default-contradictory?
  []
  (generic-operators/generic-operator base-contradictory?))





(defn- merge-base-case
  [content increment]
  (if (= content increment)
    content
    (contradiction (str (pr-str content) " != " (pr-str increment)))))

(defn default-merge
  "The default merge function returns the content if the increment is
  nothing, and the increment if the content is nothing.  Otherwise just
  checks to see if the values are the same, the default merge function
  can be extended using assign-operation."
  []
  (doto (generic-operators/generic-operator merge-base-case)
    (generic-operators/assign-operation (fn [content increment] content)
                                        any? nothing?)
    (generic-operators/assign-operation (fn [content increment] increment)
                                         nothing? any?)))

;;;;;; STOP




(defn make-system
  []
  (PropagatorSystem.
   {} {} (default-merge) (default-contradictory?) []))
