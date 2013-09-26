(ns propaganda.stm
  (:use propaganda.values)
  (:require [propaganda.generic-operators :as generic-operators]))

;; Propagator framework

(def alerting? (ref false))
(def alert-queue (ref clojure.lang.PersistentQueue/EMPTY))

(defn alert-propagators
  "Simple implementation of alerting propagators. Performed in a
  dosync."
  [propagators]
  (dosync
   (alter alert-queue into propagators)
   (when (not @alerting?)
     (ref-set alerting? true)
     (while (peek @alert-queue)
       (let [propagator (peek @alert-queue)]
         (alter alert-queue pop)
         (propagator)))
     (ref-set alerting? false))))

(def ^:private all-propagators
  "A reference to all the propagators in the runtime."
  (ref nil))

(defn alert-all-propagators!
  "Alerts all propagators in the runtime."
  []
  (alert-propagators @all-propagators))

(defprotocol Cell
  "A cell with some content. All methods on cells are performed in a
  dosync."
  (new-neighbour! [this new-neighbour]
    "Adds a new function that gets invoked on content addition to the
    cell.")
  (add-value    [this increment]
    "Adds content to the cell. If the increment is inconsistent with the
    current content, an exception is thrown.")
  (get-value    [this]
    "Gets the current content of the cell."))

(def ^:dynamic *merge*
  "The merge function used by the cells. Must be bound."
  (fn [& args]
    (throw (Exception. "Missing propaganda.stm/*merge* binding."))))

(def ^:dynamic *contradictory?*
  "The contradictory function to be used."
  (default-contradictory?))

(defn make-cell
  "Creates a new Cell with empty content. The currently bound *merge*
  and *contradictory?* are used for merging and determining if there are
  contradictions."
  []
  (let [neighbours (ref nil)
        content    (ref nothing)]
    (reify
      Cell
      (new-neighbour!
        [this new-neighbour]
        (dosync
         (when (not (contains? (set @neighbours) new-neighbour))
           (alter neighbours conj new-neighbour)
           (alter all-propagators conj new-neighbour)
           (alert-propagators [new-neighbour]))))
      (add-value
        [this increment]
        (dosync
         (let [answer (*merge* @content increment)]
           (cond
            (= answer @content)       :ok
            (*contradictory?* answer) (throw (Exception.
                                              (str "Inconsistency: "
                                                   (:reason answer))))
            :else                     (do
                                        (ref-set content answer)
                                        (alert-propagators @neighbours))))))
      (get-value
        [this]
        @content))))

(defn propagator
  "Adds a new propagator (to-do) to the neighbours and guarantees that
  it is called."
  [neighbours to-do]
  (doseq [cell neighbours]
    (new-neighbour! cell to-do))
  (alert-propagators [to-do]))

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
  (fn [& cells]
    (let [inputs (butlast cells)
          output (last cells)
          lifted-f (lift-to-cell-contents f)]
      (propagator
       inputs
       (fn [] (add-value
              output
              (apply lifted-f (map get-value inputs))))))))

(defn compound-propagator
  "Constructs a propagtor which will observe the neighbours cells and
  run to-build when their values are all different from nothing."
  [neighbours to-build]
  (let [done? (ref false)
        test (fn [] (when-not @done?
                     (when-not (some nothing? (map get-value neighbours))
                       (ref-set done? true)
                       (to-build))))]
    (propagator neighbours test)))

;; Useful propagators

(defn constant
  "Returns a propagator constructor taking cell and ensuring it always
  has the value value."
  [value]
  (function->propagator-constructor (fn [] value)))
