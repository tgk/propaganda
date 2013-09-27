# Extending propaganda with set values

This brief tutorial illustrates how to extend cell values with a new datatype, namely sets. The extension is performed in the system propagator approach, which means it can be used in both Clojure and ClojureScript.

In this example, the semantics of a set value is as follows: a cell contains some other value, or a set. If it contains a set, and you add another set, the new values is the intersection of the sets. If the intersection is empty, this is a contradiction. If a value other than a set is added to a cell, the set in the cell is checked to see if the value is in the set. If it is, the new value of the cell is the value. If not, this is a contradiction.

First, we need to get the basic dependencies

```clojure
(ns user
  (:require [propaganda.system :as system]
            [propaganda.values :as values]
            [propaganda.generic-operators :as go]
            [clojure.set :refer [intersection union difference]]))
```

Next, we create two helper functions: one for creating intersections (or contradictions), and one for checking values are in sets (or raise contradictions).

```clojure
(defn check-intersection
  [s1 s2]
  (let [i (intersection s1 s2)]
    (if (seq i)
      (if (= 1 (count i))
        (first i)
        i)
      (values/contradiction
       (format "Intersection of %s and %s is empty" s1 s2)))))

(defn check-in-set
  [e s]
  (if (contains? s e)
    e
    (values/contradiction
     (format "%s is not in %s" e s))))
```

With `check-intersection` and `check-in-set`, we are ready to write a function for extending a `merge` operator, using generic operators.

```clojure
(defn extend-merge
  [merge]
  (doto merge
    (go/assign-operation
     (fn [content increment]
       (check-in-set increment content))
     set? values/any?)
    (go/assign-operation
     (fn [content increment]
       (check-in-set content increment))
     values/any? set?)
    (go/assign-operation
     check-intersection
     set? set?)))
```

We introduce a helper function to extract cell values from a system and take our implementation for a spin:

```clojure
(defn keyword-values
  [system]
  (let [ks (->> system :values keys (filter keyword?))]
    (select-keys (:values system) ks)))

(let [my-merge (doto (values/default-merge)
                 extend-merge)
      my-contradictory (values/default-contradictory?)
      s (system/make-system my-merge my-contradictory)]
  [(-> s
       (system/add-value :cell #{:foo :bar :baz})
       (system/add-value :cell #{:foo :bar})
       keyword-values)
   (-> s
       (system/add-value :cell #{:foo :bar})
       (system/add-value :cell #{:foo})
       keyword-values)
   (-> s
       (system/add-value :cell #{:foo :bar})
       (system/add-value :cell :bar)
       keyword-values)
   (try
     (-> s
         (system/add-value :cell #{:bar})
         (system/add-value :cell #{:foo}))
     (catch Exception e e))])
```

The result of the three first expressions in the vector are cells with reasonable values. The last expression gives rise to a contradiction, which we capture and present.

```clojure
[{:cell #{:foo :bar}}
 {:cell :foo}
 {:cell :bar}
 #<ExceptionInfo clojure.lang.ExceptionInfo: Inconsistency {:contradiction #propaganda.values.Contradiction{:reason "Intersection of #{:bar} and #{:foo} is empty"}}>]
```

That's all that is needed for basic set support. To help you set up relations between sets, here is a basic union relation. Creating other relations can be done in a similar manner.

```clojure
(defn union-relation
  [system s1 s2 result]
  (-> system
     ((system/function->propagator-constructor union) s1 s2 result)
     ((system/function->propagator-constructor difference) result s1 s2)
     ((system/function->propagator-constructor difference) result s2 s1)))
```

Have fun exploring sets!
