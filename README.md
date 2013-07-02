# propaganda

The propaganda library is a Clojure implementation of the propagator computational model described in [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215). The aim of the library is to be easy to use and reason about, yet extensible.

## Artifact

Leiningen dependency:

    [propaganda "0.0.1"]

## Tutorial

The `gh-pages` branch of this project contains a brief explanation of the basics of the propagator computational model. The page can be visited [here](http://tgk.github.io/propaganda/). The [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215) contains a more in-depth explanation of how propagation can be implemented.

To use the propaganda library, you need to define a merge, function, create cells and set up propagators. In this short example we just use the default merger function, we define the square and square-root propagator and set up relations beween simple cells. An example where values are intervals and merging is performed by taking the intersection of intervals can be found in `examples/building_height.clj`

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
    ;; => 100

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



## Motivation

The objective of this project is to create an extinsible propagator library for Clojure. Propagators define a computational model not completely unlike that found in FRP, but with some differences. They are described in the article "The Art of the Propagator".

Along with the library itself, the project should supply documentation of the API, good examples and tutorials.

I have not previously worked with propagtors, so this will also be an exploration for me.

## Backlog

- Add value provence datatype and merge function
- More examples on tgk.github.io/propaganda
- Add TMS datatype and merge function
