# propaganda

<img src="https://raw.github.com/tgk/propaganda/master/doc/propaganda.png" alt="Punching values through the system" title="Propaganda" align="right" />

The propaganda library is a Clojure implementation of the propagator computational model described in [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215). The aim of the library is to be easy to use and reason about, yet extensible.

There is a Clojars release containing an implementation that works in both Clojure and ClojureScript.

The leiningen dependency is

    [propaganda "0.1.2"]

## Tutorial

The `gh-pages` branch of this project contains a brief explanation of the basics of the propagator computational model. The page can be visited [here](http://tgk.github.io/propaganda/). [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215) contains a more in-depth explanation of how propagation can be implemented.

To use the propaganda library, you need to define a merge, function, create cells and set up propagators. In this short example we just use the default merger function, we define the square and square-root propagator and set up relations beween simple cells. An example where values are intervals and merging is performed by taking the intersection of intervals can be found in [`examples/building_height.clj`](https://github.com/tgk/propaganda/blob/master/examples/building_height.clj).


```
(use 'propaganda.stm)
(use 'propaganda.values)
```

`default-merge` will give us a merger that will merge
nothing with anything, but will enforce that anything else
that is attempted to be merged will return a contradiction
```
(def my-merge
  (default-merge))
```

`nothing` can be merged with `nothing` and will return `nothing`
```
(my-merge nothing nothing)
;; => #<Object ...>
```

Anything else will be the result of the merge
```
(my-merge nothing 1)
;; => 1
(my-merge 2 nothing)
;; => 2
(my-merge 1 1)
;; => 1
```

... unless it gives rise to a contradiction
```
(my-merge 1 2)
;; => #propaganda.core.Controdiction{:reason "1 != 2"}
```

The `function->propagator-constructore` can be used for setting up
simple one way relations
```
(def squarer
  (function->propagator-constructor
   (fn [val] (* val val))))

(def sqrter
  (function->propagator-constructor
   (fn [val] (Math/sqrt val))))
```

... which can be extended to go both ways
```
(defn quadratic
  [x x-squared]
  (squarer x x-squared)
  (sqrter  x-squared x))
```

We can not construct cells and set up the quadratic relations to read
the squared of a number in our system:
```
(let [x (make-cell)
      x-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic x x-squared)
    (add-content x 10.0)
    (get-content x-squared)))
;; => 100.0
```

Or the square-root, depending on the input from the user
```
(let [y (make-cell)
      y-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic y y-squared)
    (add-content y-squared 1764.0)
    (get-content y)))
;; => 42.0
```

We will be warned of any inconsistencies in our system when adding
content
```
(let [z (make-cell)
      z-squared (make-cell)]
  (binding [*merge* my-merge]
    (quadratic z z-squared)
    (add-content z 10.0)
    (add-content z-squared 123.0)))
;; Exception: Inconsistency: 100.0 != 123.0
```

## Motivation

The objective of this project is to create an extinsible propagator library for Clojure. Propagators define a declarative computational model. They are described in the article [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215).

Along with the library itself, the project should supply documentation of the API, good examples and tutorials.

I have not previously worked with propagtors, so this will also be an exploration for me.

## Thanks to

[Ragnar Dahlen](https://github.com/ragnard/) and [Kasper Langer](https://github.com/kasperlanger) for feedback on the library.
