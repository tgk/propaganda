(ns propaganda.core
  (:require [propaganda.generic-operators :as generic-operators]))

;; Propagator framework

(def nothing (Object.))

(defn nothing?
  [thing]
  (= nothing thing))

(defn any?
  [thing]
  (not= nothing thing))

;; Insigth: This method works, but could potentially blow the stack. An
;; alternative strategy would be to have two refs: alerting-propagators
;; and propagator-queue. alert-propagators should add the propagators to
;; the queue, check to see if af consume loop is running, and if not,
;; start one
(defn alert-propagators
  "Simple implementation of alerting propagators. The original article
  seems to indicate that scheduling should be used instead of just
  aggressively executing the propagators."
  [propagators]
  (doseq [propagator propagators]
    (propagator)))

(defprotocol Cell
  (new-neighbour! [this new-neighbour])
  (add-content    [this increment])
  (get-content    [this]))

;; *merge* is introduced as the merging function

(defrecord Contradiction [reason])

(defn contradiction
  [reason]
  (Contradiction. reason))

(defn contradictory?
  [x]
  (isa? (class x) Contradiction))

(defn- merge-base-case
  [content increment]
  (if (= content increment)
    content
    (contradiction (str (pr-str content) " != " (pr-str increment)))))

(defn default-merge
  "The default merge function returns the content if the increment is
  nothing, and the increment if the content is nothing. This aligns with
  the cond behaviour in add-content from the previous sections.
  Otherwise just checks to see if the values are the same, the default
  merge function can be extended using assign-operation."
  []
  (doto (generic-operators/generic-operator merge-base-case)
    (generic-operators/assign-operation (fn [content increment] content)
                                        any? nothing?)
    (generic-operators/assign-operation (fn [content increment] increment)
                                         nothing? any?)))

(def ^:dynamic *merge*
  "The merge function used by the cells. Must be bound."
  (fn [& args]
    (throw (Exception. "Missing propaganda.core/*merge* binding."))))

(defn make-cell
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
           (alert-propagators [new-neighbour]))))
      (add-content
        [this increment]
        (dosync
         (let [answer (*merge* @content increment)]
           (cond
            (= answer @content)     :ok
            (contradictory? answer) (throw (Exception.
                                            (str "Inconsistency: "
                                                 (:reason answer))))
            :else                   (do
                                      (ref-set content answer)
                                      (alert-propagators @neighbours))))))
      (get-content
        [this]
        @content))))

(defn propagator
  "Adds a new propagator (to-do) to the neighbours and guarantees that
  it is called (although adding it should have that side-effect, but not
  doing it causes a failure - there is something I haven't thought
  through)"
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
       (fn [] (add-content
              output
              (apply lifted-f (map get-content inputs))))))))

(defn compound-propagator
  "Constructs a propagtor which will observe the neighbours cells and
  run to-build when their values are all different from nothing."
  [neighbours to-build]
  (let [done? (ref false)
        test (fn [] (when-not @done?
                     (when-not (some nothing? (map get-content neighbours))
                       (ref-set done? true)
                       (to-build))))]
    (propagator neighbours test)))

;; Useful propagators

(defn constant
  [value]
  (function->propagator-constructor (fn [] value)))
