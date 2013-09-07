(ns propaganda.system
  (:require [propaganda.generic-operators :as generic-operators]))

(defrecord PropagatorSystem
    [values propagators merge contradictory? alert-queue freezing?])

(defprotocol PropagatorSystemProtocol
  (add-value             [this cell value])
  (get-value             [this cell])
  (add-propagator        [this cells f])
  (cool                  [this])
  (stabile?              [this])
  (alert-all-propagators [this]))

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

;;;;;;

(defn- update-keys
  [m ks f & args]
  (reduce (fn [m k] (assoc m k (apply f (get m k) args)))
          m
          ks))

(defn- disturb-system
  "Helper for adding a value to the system, without cooling it."
  [system cell value]
  (let [content (get-value system cell)
        answer ((:merge system) content value)]
    (cond

     (= answer content)
     system

     ((:contradictory? system) answer)
     (throw (ex-info "Inconsistency" {:contradiction answer}))

     :else
     (-> system
         (assoc-in [:values cell] answer)
         (update-in [:alert-queue] concat (get-in system [:propagators cell]))))))

;; freezing? is used to avoid deep recursion. Discuss if there are any
;; states, e.g. on exception, that might ruin this approach
(defn- freeze
  [system]
  ;; only freeze the system if some other loop isn't marked as freezing the system
  (if (:freezing? system)
    system
    (assoc
        (loop [system (assoc system :freezing? true)]
          (if (stabile? system)
            system
            (recur (cool system))))
      :freezing? false)))

(defn- all-propagators
  [system]
  (set (mapcat second (:propagators system))))

(extend-type PropagatorSystem

  PropagatorSystemProtocol

  (add-value [this cell value]
    (-> this
        (disturb-system cell value)
        freeze))

  (get-value [this cell]
    (get-in this [:values cell] nothing))

  (add-propagator [this cells f]
    (-> this
        (update-in [:propagators] #(update-keys % cells conj f))
        (update-in [:alert-queue] conj f)
        freeze))

  (stabile? [this]
    (empty? (:alert-queue this)))

  ;; If you are interested in values as the system ticks along, hook
  ;; into cool
  (cool [this]
    (if-let [[f & t] (:alert-queue this)]
      (f (assoc this :alert-queue t))
      this))

  (alert-all-propagators [this]
    (-> this
        (update-in [:alert-queue] concat (all-propagators this))
        freeze)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-system
  ([]
     (make-system (default-merge) (default-contradictory?)))
  ([merge contradictory?]
     (PropagatorSystem.
      {} {} merge contradictory? [] false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lift-to-cell-contents
  "Returns a safe-guarded version of f which ensures that all arguments
  are different than nothing."
  [f]
  (fn [& args]
    (if (some nothing? args)
      nothing
      (apply f args))))

(defn function->propagator-constructor
  "Returns a propagtor constructor which will lift the content of f
  applied to the first cells to the last cell."
  [f]
  (fn [system & cells]
    (let [inputs (butlast cells)
          output (last cells)
          lifted-f (lift-to-cell-contents f)]
      (add-propagator
       system
       inputs
       (fn [system]
         (add-value
          system
          output
          (apply lifted-f (map (partial get-value system) inputs))))))))

(defn constant
  [value]
  (function->propagator-constructor (fn [] value)))
