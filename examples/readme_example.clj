(require '(propaganda [core :as propaganda]))

;; default-merge will give us a merger that will merge
;; propaganda/nothing with anything, but will enforce that anything else
;; that is attempted to be merged will return a contradiction
(def my-merge
  (propaganda/default-merge))

;; nothing can be merged with nothing and will return nothing
(my-merge propaganda/nothing propaganda/nothing)
;; => #<Object ...>

;; anything else will be the result of the merge
(my-merge propaganda/nothing 1)
;; => 1
(my-merge 2 propaganda/nothing)
;; => 2
(my-merge 1 1)
;; => 1

;; ... unless it gives rise to a contradiction
(my-merge 1 2)
;; => #propaganda.core.Controdiction{:reason "1 != 2"}


;; the function->propagator-constructore can be used for setting up
;; simple one way relations
(def squarer
  (propaganda/function->propagator-constructor
   (fn [val] (* val val))))

(def sqrter
  (propaganda/function->propagator-constructor
   (fn [val] (Math/sqrt val))))

;; ... which can be extended to go both ways
(defn quadratic
  [x x-squared]
  (squarer x x-squared)
  (sqrter  x-squared x))


;; we can not construct cells and set up the quadratic relations to read
;; the squared of a number in our system:
(let [x (propaganda/make-cell)
      x-squared (propaganda/make-cell)]
  (binding [propaganda/*merge* my-merge]
    (quadratic x x-squared)
    (propaganda/add-content x 10.0)
    (propaganda/get-content x-squared)))
;; => 100.0

;; or the square-root, depending on the input from the user
(let [y (propaganda/make-cell)
      y-squared (propaganda/make-cell)]
  (binding [propaganda/*merge* my-merge]
    (quadratic y y-squared)
    (propaganda/add-content y-squared 1764.0)
    (propaganda/get-content y)))
;; => 42.0

;; we will be warned of any inconsistencies in our system when adding
;; content
(let [z (propaganda/make-cell)
      z-squared (propaganda/make-cell)]
  (binding [propaganda/*merge* my-merge]
    (quadratic z z-squared)
    (propaganda/add-content z 10.0)
    (propaganda/add-content z-squared 123.0)))
;; Exception: Inconsistency: 100.0 != 123.0
