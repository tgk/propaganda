(ns propaganda.support-values
  (:require clojure.set
            [propaganda.generic-operators :as go]

            ;; For testing purposes
            [propaganda.core :as p]))

(defrecord Supported
    [value support-set])

(defn supported
  [value support]
  (Supported. value (set [support])))

(defn supported?
  [x]
  (isa? (class x) Supported))

(defn more-informative-support?
  "Returns true if the support-set for support-2 contains strictly more
  information than the support-set in support-1."
  [support-1 support-2]
  (and (not= (:support-set support-1) (:support-set support-2))
       (clojure.set/subset? (:support-set support-1) (:support-set support-2))))

(defn merge-supports
  [& supports]
  (apply clojure.set/union (map :support-set supports)))

(defn implies?
  [merge-operator val-1 val-2]
  (= val-1 (merge-operator val-1 val-2)))

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
  [generic-contradictory?-operator]
  (go/assign-operation
   generic-contradictory?-operator
   (fn [support]
     (generic-contradictory?-operator (:value support)))
   supported?))

#_(let [my-merge (p/default-merge)
      my-contradictory? (p/generic-contradictory?)]
  (extend-merge my-merge)
  (extend-contradictory? my-contradictory?)
  (binding [p/*merge* my-merge
            p/*contradictory?* my-contradictory?]
    (let [cell (p/make-cell)]
      (p/add-content cell (supported 42 :i-say-so))
      (p/add-content cell (Supported. 42 #{:i-say-so :so-do-i}))
      #_(p/add-content cell (Supported. 43 #{:i-say-so :so-do-i}))
      (p/get-content cell))))
