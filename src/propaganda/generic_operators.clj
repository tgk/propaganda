(ns propaganda.generic-operators)

(defn all-preds?
  "Returns true iff (pred vals) is truthy for all preds paired with
  vals."
  [preds vals]
  (and (= (count preds) (count vals))
       (every?
        identity
        (for [[pred val] (map vector preds vals)] (pred val)))))

(defn val-with-predicates
  "Returns the first val in pred&vals seq where all preds saitsfy args."
  [pred&vals & args]
  (second
   (first
    (filter
     (fn [[preds _]] (all-preds? preds args))
     pred&vals))))

(defn execute-op
  "Choses an operation from pairs of predicates and operators and
  executes it on args. Executes default on args if no predicates match."
  [default pred&ops & args]
  (if-let [op (apply val-with-predicates pred&ops args)]
    (apply op args)
    (apply default args)))

(defn generic-operator
  "Returns a generic operation with default operator default. Generic
  operators can be extended using assign-operation. For example, we can
  define a generic plus operator that works on numbers and vectors:

  (let [plus (generic-operator +)]
    (doto plus
      (assign-operation concat vector? vector?))
    [(plus 1 2)
     (plus [1 2 3] [4 5])])
  ;; => [3 [1 2 3 4 5]]"
  [default]
  (let [pred&ops (atom nil)]
    (with-meta
      (fn [& args]
        (apply execute-op default @pred&ops args))
      {:pred&ops pred&ops})))

(defn assign-operation
  "Alters generic-operator, adding operation given preds."
  [generic-operator operation & preds]
  (let [preds&ops (:pred&ops (meta generic-operator))]
    (swap! preds&ops conj [preds operation])))
