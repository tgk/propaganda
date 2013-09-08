(ns propaganda.values
  (:require [propaganda.generic-operators :as generic-operators]))

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
