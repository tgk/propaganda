(ns propaganda.tms
  (:require [clojure.set]
            [propaganda.core :as propaganda]
            [propaganda.support-values :as support-values]
            [propaganda.generic-operators :as go]))

(defrecord TruthMaintenanceSystem [supported-values])

(defn make-tms
  [& supported-values]
  (TruthMaintenanceSystem. (set supported-values)))

(defn tms?
  [thing]
  (isa? (class thing) TruthMaintenanceSystem))

;; Passing merge operator around - might just want to use propaganda/*merge* ?
(defn tms-assimilate-one
  "Adds support to the tms, but only if it adds a more precise
  value (that is, a different value than the existing) when combined
  with the existing values. Removes old values that do not add to the
  information anymore."
  [merge-operator tms support]
  ;; is the existing information better that the new support?
  (if (some (fn [old-support]
              (support-values/subsumes?
               merge-operator old-support support))
            (:supported-values tms))
    ;; then just return old tms
    tms
    ;; otherwise, find support values that can be removed as the new
    ;; information is better than what they bring to the table
    (let [subsumed (filter (fn [old-support]
                             (support-values/subsumes?
                              merge-operator support old-support))
                           (:supported-values tms))]
      (apply make-tms
             (clojure.set/union
              (clojure.set/difference (:supported-values tms) subsumed)
              #{support})))))

(defn tms-assimilate
  "Incorporate stuff into tms. Stuff can be nothing, a supported value
  or a tms."
  [merge-operator tms stuff]
  (cond
   (propaganda/nothing? stuff) tms
   (support-values/supported? stuff) (tms-assimilate-one tms stuff)
   (tms? stuff) (reduce (partial tms-assimilate-one merge-operator)
                        tms
                        (:supported-values stuff))
   :else (throw (Exception. "Should never happen"))))

;; Premises

;;;;; A premise being in or out might belong in supported values?

(def premises-out
  (ref #{}))

(defn premise-in?
  [premise]
  (not (contains? @premises-out premise)))

(defn mark-premise-in!
  [premise]
  (alter premises-out clojure.set/difference #{premise}))

(defn mark-premise-out!
  [premise]
  (alter premises-out conj premise))

;;; No premise-no-good collection for now

(defn process-no-good!
  "Reacts to the premises that are no-good. Just throw an exception for
  now, but should allow the user to change the worldview."
  [premises]
  (throw (Exception.
          (str "The current worldview contains a conflict. "
               "The following premises are up to no good: "
               premises))))

(defn all-premises-in?
  "Checks that all premises for thing are valid in the current
  worldview. thing should be a supported value or a seq of premises."
  [thing]
  (if (support-values/supported? thing)
    (all-premises-in? (:support-set thing))
    (every? premise-in? thing)))

(defn strongest-consequence
  "Returns the most informative consequence of the current worldview."
  [merge-operator tms]
  (let [relevant-supports (filter all-premises-in? (:supported-values tms))]
    (reduce merge-operator propaganda/nothing relevant-supports)))

(defn check-consistent!
  [contradictory? support]
  (when (contradictory? support)
    (process-no-good! (:support-set support))))

;; Extending merge

(defn extend-merge
  [generic-merge-operator contradictory?]
  (go/assign-operation generic-merge-operator
                       ;; content increment
                       (fn [tms1 tms2]
                         (let [candidate (tms-assimilate tms1 tms2)
                               consequence (strongest-consequence candidate)]
                           (check-consistent! contradictory? consequence)
                           (tms-assimilate generic-merge-operator candidate consequence)))
                       tms? tms?))

;; Querying

(defn tms-query
  [contradictory? tms]
  (let [answer (strongest-consequence tms)
        better-tms (tms-assimilate tms answer)]
    (if (not= tms better-tms)
      :do-nothing ;; The original article alters the tms-values of the
                  ;; tms, but that would require a ref in the tms, and
                  ;; it would no longer be immutable. Let's try not to
                  ;; do that, and see what happens
      )
    (check-consistent! contradictory? answer)
    answer))

;; Kicking out and bringing in

;;; Some fairly aggresive strategies being used here...

(defn kick-out!
  [premise]
  (let [was-in? (premise-in? premise)]
    (mark-premise-out! premise)
    (when was-in?
      (propaganda/alert-all-propagators!))))

(defn bring-in!
  [premise]
  (let [was-out? (not (premise-in? premise))]
    (mark-premise-in! premise)
    (when was-out?
      (propaganda/alert-all-propagators!))))
