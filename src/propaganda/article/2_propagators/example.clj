(ns propaganda.article.2-propagators.example)

(def nothing (Object.))

(defn nothing?
  [thing]
  (= nothing thing))

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
         (cond
          (nothing? increment)   :ok
          (nothing? @content)    (do (ref-set content increment)
                                     (alert-propagators @neighbours))
          :else                  (when (not (= increment @content))
                                   (throw
                                    (Exception. "increment did not match content"))))))
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

(defn conditional
  "Adds propagator which ensures that output if bound to if-true if p,
  and to if-false if not."
  [p if-true if-false output]
  (propagator [p if-true if-false]
              (fn []
                (let [pred (get-content p)]
                  (when-not (nothing? pred)
                    (add-content output
                                 (if pred
                                   (get-content if-true)
                                   (get-content if-false))))))))

(defn switch
  "Adds propagator which ensures that output is bound to if-true when
  p."
  [p if-true output]
  (conditional p if-true (make-cell) output))

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

;; propagator constructors

(def adder      (function->propagator-constructor +))
(def subtractor (function->propagator-constructor -))
(def multiplier (function->propagator-constructor *))
(def divider    (function->propagator-constructor /))

(def absolute-value (function->propagator-constructor
                     (fn [v] (if (< v 0) (- v) v))))
(def <? (function->propagator-constructor <))
(def inverter (function->propagator-constructor not))

(defn constant
  [value]
  (function->propagator-constructor (fn [] value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Heron

(defn heron-step
  "Construct a heron step machine with input cells x and g, and output
  cell h."
  [x g h]
  (compound-propagator
   [x g]
   (fn []
     (let [x-over-g   (make-cell)
           g+x-over-g (make-cell)
           two        (make-cell)]
       (divider x g x-over-g)
       (adder g x-over-g g+x-over-g)
       ((constant 2) two)
       (divider g+x-over-g two h)))))

;; Single Heron step test
(comment
  (let [x            (make-cell)
        guess        (make-cell)
        better-guess (make-cell)]
    (heron-step x guess better-guess)
    (add-content x 2)
    (add-content guess 1.4)
    (get-content better-guess)))
;; => 1.4142857142857141

(defn good-enuf?
  "Creates propagator to determines if the square of guess in cell g is
  within epsilon of x and stores the boolean result in the cell done."
  [g x done]
  (compound-propagator
   [g x]
   (fn []
     (let [epsilon         (make-cell)
           g-squared       (make-cell)
           x-g-squared     (make-cell)
           abs-x-g-squared (make-cell)]
       ((constant 0.00000001) epsilon)
       (multiplier g g g-squared)
       (subtractor x g-squared x-g-squared)
       (absolute-value x-g-squared abs-x-g-squared)
       (<? abs-x-g-squared epsilon done)))))

;; Test of good-enuf?
(comment
  (let [x    (make-cell)
        g    (make-cell)
        done (make-cell)]
    ((constant 1) x)
    ((constant 1.000000000001) g)
    (good-enuf? g x done)
    (get-content done)))

(defn sqrt-iter
  "Creates a compound propagator for testing if a result is good
  enough. Also sets up propagators for the case where it is not, and
  recursively creates a new sqrt-iter machine."
  [x g answer]
  (compound-propagator
   [x g]
   (fn []
     (let [done          (make-cell)
           not-done      (make-cell)
           x-if-not-done (make-cell)
           g-if-not-done (make-cell)
           new-g         (make-cell)]
       (good-enuf? g x done)
       (switch done g answer)
       (inverter done not-done)
       (switch not-done x x-if-not-done)
       (switch not-done g g-if-not-done)
       (heron-step x-if-not-done g-if-not-done new-g)
       (sqrt-iter x-if-not-done new-g answer)))))

(defn sqrt-network
  [x answer]
  (compound-propagator
   [x]
   (fn []
     (let [one (make-cell)]
       ((constant 1) one)
       (sqrt-iter x one answer)))))


;; Running it all
(comment
  (double
   (let [x (make-cell)
         answer (make-cell)]
     ((constant 2) x)
     (sqrt-network x answer)
     (get-content answer))))
;; => 1.41421356237469
