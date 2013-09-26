(ns propaganda.system
  (:require [propaganda.values :as values]))

(defrecord PropagatorSystem
    [values propagators merge contradictory? alert-queue freezing?])

(defprotocol PropagatorSystemProtocol
  "A propagator system that contains a set of cells with values. Any
  clojure value can be used as a cell identifier. Cells do not need to
  be initialised. Their value defaults to
  propaganda.values/nothing. When a value is added to the system,
  propagators are alerted, bringing the system to an unstabile
  state.

  The system is cooled down until it reaches a stabile state again. If
  the :audit? key is true in the metadata of the system, each unstabile
  system is stored under the :prev key of the systems."
  (add-value             [this cell value])
  (get-value             [this cell])
  (add-propagator        [this cells f])
  (cool                  [this])
  (stabile?              [this])
  (alert-all-propagators [this]))

(defn- update-keys
  "Applies f to the values of m under ks."
  [m ks f & args]
  (reduce (fn [m k] (assoc m k (apply f (get m k) args)))
          m
          ks))

(defn- disturb-system
  "Helper for adding a value to the system, without cooling it."
  [system cell value]
  (let [content (get-value system cell)
        answer ((:merge system) content value)
        new-system (cond

                    (= answer content)
                    system

                    ((:contradictory? system) answer)
                    (throw (ex-info "Inconsistency" {:contradiction answer}))

                    :else
                    (-> system
                        (assoc-in [:values cell]
                                  answer)
                        (update-in [:alert-queue]
                                   concat
                                   (get-in system [:propagators cell]))))]
    (if (:audit? (meta system))
      (with-meta new-system {:audit? true :prev system})
      new-system)))

(defn- freeze
  "Freezes the system down until it reaches a stabile state. If the
  system is already in the process of being cooled down, nothing is done
  to the system."
  [system]
  (if (:freezing? system)
    system
    (assoc
        (loop [system (assoc system :freezing? true)]
          (if (stabile? system)
            system
            (recur (cool system))))
      :freezing? false)))

(defn- all-propagators
  "Returns all propagators of the system."
  [system]
  (set (mapcat second (:propagators system))))

(extend-type PropagatorSystem

  PropagatorSystemProtocol

  (add-value [this cell value]
    (-> this
        (disturb-system cell value)
        freeze))

  (get-value [this cell]
    (get-in this [:values cell] values/nothing))

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

(defn make-system
  "Creates a new system. If no merge and contradictory? is given, the
  default versions from the values namespace are used. To merge values,
  e.g. interval, supply a custom merge."
  ([]
     (make-system (values/default-merge)
                  (values/default-contradictory?)))
  ([merge contradictory?]
     (PropagatorSystem.
      {} {} merge contradictory? [] false)))

(defn lift-to-cell-contents
  "Returns a safe-guarded version of f which ensures that all arguments
  are different than nothing."
  [f]
  (fn [& args]
    (if (some values/nothing? args)
      values/nothing
      (apply f args))))

(defn function->propagator-constructor
  "Returns a propagtor constructor which will lift the content of f
  applied to the first cells to the last cell. The propagator
  constructor returned accepts a system and the cells. An altered system
  is returned."
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
  "Returns a propagator constructor taking system and a cell, ensuring
  it always has the value value. Returns a new system."
  [value]
  (function->propagator-constructor (fn [] value)))
