(use 'propaganda.stm)
(use 'propaganda.values)

;; default-merge will give us a merger that will merge
;; nothing with anything, but will enforce that anything else
;; that is attempted to be merged will return a contradiction
(def my-merge
  (default-merge))

;; nothing can be merged with nothing and will return nothing
(my-merge nothing nothing)
;; => #<Object ...>

;; anything else will be the result of the merge
(my-merge nothing 1)
;; => 1
(my-merge 2 nothing)
;; => 2
(my-merge 1 1)
;; => 1

;; ... unless it gives rise to a contradiction
(my-merge 1 2)
;; => #propaganda.core.Controdiction{:reason "1 != 2"}


;; the function->propagator-constructore can be used for setting up
;; simple one way relations
(def squarer
  (function->propagator-constructor
   (fn [val] (* val val))))

(def sqrter
  (function->propagator-constructor
   (fn [val] (Math/sqrt val))))

;; ... which can be extended to go both ways
(defn quadratic
  [x x-squared]
  (squarer x x-squared)
  (sqrter  x-squared x))


;; we can not construct cells and set up the quadratic relations to read
;; the squared of a number in our system:
(let [x (make-cell)
      x-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic x x-squared)
    (add-value x 10.0)
    (get-value x-squared)))
;; => 100.0

;; or the square-root, depending on the input from the user
(let [y (make-cell)
      y-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic y y-squared)
    (add-value y-squared 1764.0)
    (get-value y)))
;; => 42.0

;; we will be warned of any inconsistencies in our system when adding
;; content
(let [z (make-cell)
      z-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic z z-squared)
    (add-value z 10.0)
    (add-value z-squared 123.0)))
;; Exception: Inconsistency: 100.0 != 123.0
