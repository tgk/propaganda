(ns propaganda.support-values
  (:require clojure.set
            [propaganda.generic-operators :as go]))

(defrecord Supported
    [value support-set])

(defn setify
  [v]
  (if (set? v)
    v
    #{v}))

(defn supported
  [value support]
  (Supported. value (setify support)))

(defn supported?
  [x]
  (isa? (type x) Supported))

(defn- more-informative-support?
  "Returns true if the support-set for support-1 contains strictly more
  information than the support-set in support-2."
  [support-1 support-2]
  (and (not= (:support-set support-1) (:support-set support-2))
       (clojure.set/subset? (:support-set support-2) (:support-set support-1))))

(defn- merge-supports
  "Returns the merge of the supporting sets for supports."
  [& supports]
  (apply clojure.set/union (map :support-set supports)))

(defn- implies?
  "Returns true if the val-1 information implies val-2 is val-1."
  [merge-operator val-1 val-2]
  (= val-1 (merge-operator val-1 val-2)))

(defn subsumes?
  "Returns true iff the information in support-2 is deducible from
  support-1."
  [merge-operator support-1 support-2]
  (and (implies? merge-operator (:value support-1) (:value support-2))
       (clojure.set/subset? (:support-set support-2) (:support-set support-1))))

(defn extend-merge
  "Extends the generic merge operator with support for supported
  values."
  [generic-merge-operator]
  (go/assign-operation
   generic-merge-operator
   ;;  content   increment
   (fn [support-1 support-2]
     (let [val-1 (:value support-1)
           val-2 (:value support-2)
           val-merge (generic-merge-operator val-1 val-2)]
       (cond
        (= val-merge val-1) (if (implies? generic-merge-operator
                                          val-2 val-merge)
                              ;; Confirmation of existing
                              (if (more-informative-support?
                                   support-2 support-1)
                                support-2
                                support-1)
                              ;; New information is not interesting
                              support-1)
        (= val-merge val-2) support-2 ;; New information overrides
        :else               (Supported. val-merge
                                        (merge-supports support-1 support-2)))))
   supported? supported?))

(defn extend-contradictory?
  "Extends the generic contradictory? operator with support for
  supported values."
  [generic-contradictory?-operator]
  (go/assign-operation
   generic-contradictory?-operator
   (fn [support]
     (generic-contradictory?-operator (:value support)))
   supported?))

(defn supported-unpacking
  "Returns a function that will take supported arguments, apply f to
  them and return a supported value with their merged supporting sets."
  [f]
  (fn [& args]
    (supported
     (apply f (map :value args))
     (apply merge-supports args))))

(defn ->supported
  "Returns thing if it is already a supported value. Creates new
  supported value with an empty support set otherwise."
  [thing]
  (if (supported? thing)
    thing
    (supported thing #{})))
