(ns propaganda.values
  (:require [propaganda.generic-operators :as generic-operators]))

(def nothing
  "The value representing no content of a cell."
  ::nothing)

(defn nothing?
  "Determins if thing is nothing."
  [thing]
  (= nothing thing))

(defn any?
  "Determins if thing is different than nothing."
  [thing]
  (not= nothing thing))

(defrecord Contradiction [reason])

(defn contradiction
  "Creates a new contradiction with the given reason, a string."
  [reason]
  (Contradiction. reason))

(defn- base-contradictory?
  "Base version of the contradiction function."
  [x]
  (isa? (type x) Contradiction))

(defn default-contradictory?
  "Constructs a default contradictory? generic operator that can be
  extended using assign-operation from the generic-operators namespace."
  []
  (generic-operators/generic-operator base-contradictory?))

(defn- merge-base-case
  "Base version of the merge function. Only identical content and
  increments does not give rise to a contradiction."
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
