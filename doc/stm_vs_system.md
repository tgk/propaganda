# STM vs. System propagation

The original [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215) paper uses an approach in which cells are created and maintained in the runtime of the environment hosting the implementation. This approach lends itself very nicely to Clojure's STM, as coordination between cells and propagators must happen in a synchronised manner. The implementation in the `propaganda.stm` namespace follows this approach.

The approach does not directly translate to runtimes in which there are weaker synchronisation mechanisms, such as the javascript runtime. The propaganda library therefore contains a novel approach in which all values of a system is kept in an immutable datastructure, called the *system*. When values are added to cells, a new system is returned with the value added, or a contradiction is raised if the value contradicts facts already in the system. The implementation in the `propaganda.system` namespace follows this approach.

This document outlines how the two different approaches can be used to solve the building height problem (also known as [the barometer question](http://en.wikipedia.org/wiki/Barometer_question)).

## The building height problem

We wish to determine the height of a building, using only a barometer. We are going to use three different approaches to do so:

- _Fall duration_ We are going to drop the barometer from the top of the building, measure the time until the barometer hits the ground, and estimate the height using the gravitational force.

- _Barometer shadow_ We will measure the shadow of the building, the height of the barometer and the length of the barometers shadow. The barometer will represent a scaled version of the building, and by finding the ratio between the two shadows, we can apply the same ratio to the height of the barometer to find the height of the building.

- _Bribing the superintendent_ We will give the barometer to the superintendent in exchange for the information.

The two first approaches will have a lot of error associated with them, represented by interval values of our cells. As we add information, it will feed back into these cells to improve the estimates of our input variables.

## STM building height implementation

First, we need the basic dependencies for having interval values and STM propagation.

```clojure
(use 'propaganda.stm)
(use 'propaganda.values)
(use '[propaganda.intervals.common :exclude [extend-merge]])
(use 'propaganda.intervals.stm)
```

We create a helper function for setting up a fall duration relation between cells. `(fall-duration t h)` will create the relationship between time `t` in seconds and heigth `h` subject to some uncertainty on the gravitational force.

```clojure
(defn fall-duration
  [t h]
  (compound-propagator
   [t]
   (fn []
     (let [g          (make-cell)
           one-half   (make-cell)
           t-squared  (make-cell)
           gt-squared (make-cell)]
       ((constant (make-interval 9.789 9.832)) g)
       ((constant 0.5) one-half)
       (quadratic t t-squared)
       (product g t-squared gt-squared)
       (product one-half gt-squared h)))))
```

We also introduce a helper for the shadow relations, called `similar-triangles`. `(similar-triangles s-ba h-ba s h)` create the relation described in the previous section between the shadow and height of the barometer, and the shadow and height of the building.

```clojure
(defn similar-triangles
  [s-ba h-ba s h]
  (compound-propagator
   [s-ba h-ba s]
   (fn []
     (let [ratio (make-cell)]
       (product s-ba ratio h-ba)
       (product s ratio h)))))
```

We can now create a merge function that takes intervals into account, set up our cells and add the relations between them.

```clojure
(let [custom-merge (doto (default-merge) extend-merge)]
   (binding [*merge* custom-merge]
     (let [building-height (make-cell)

           fall-time       (make-cell)

           barometer-height (make-cell)
           barometer-shadow (make-cell)
           building-shadow  (make-cell)]

       (fall-duration fall-time building-height)
       (add-value fall-time (make-interval 2.9 3.1))

       (similar-triangles barometer-shadow barometer-height
                          building-shadow building-height)
       (add-value building-shadow (make-interval 54.9 55.1))
       (add-value barometer-height (make-interval 0.3 0.32))
       (add-value barometer-shadow (make-interval 0.36 0.37))

       (add-value building-height 45.0)

       [(get-value building-height)
        (get-value building-shadow)
        (get-value barometer-height)
        (get-value barometer-shadow)
        (get-value fall-time)])))
```

As you can see, most of our input are intervals, as there will be some uncertainty on our observations (we are measuring the real world, after all). The output of the expression is given below. Notice how the intervals get refined on our input values. For example, we now have a much better estimate on the fal time than we had before (from [2.9, 3.1] to [3.026, 3.032]).

```clojure
[45
 {:lo 54.9, :hi 55.1}
 {:lo 0.3, :hi 0.30327868852459017}
 {:lo 0.366, :hi 0.37}
 {:lo 3.025522031629098, :hi 3.0321598338046556}]
```

There are several annoying things about the example above:

- We have to use bindings to set `*merge*`, the merging function that understands intervals.
- Steps are performed in (implicit) `dosync`s.
- We are (implictly) putting our propagators in global vars where they will never be garbage collected from.
- We are unable to branch out. If we change the value of a cell, it will remain changed. We can't undo add a value to a cell.

The system approach solves all of these problems.

## System building height implementation

To switch to the system approach, we need the `system` namespaces where we used `stm` before.

```clojure
(use 'propaganda.system)
(use 'propaganda.values)
(use '[propaganda.intervals.common :exclude [extend-merge]])
(use 'propaganda.intervals.system)
```

The functions setting up our relations now have to take the system as a parameter and return an altered system, as opposed to registering global propagators.

```clojure
(defn fall-duration
  [system t h]
  (let [g          (gensym)
        one-half   (gensym)
        t-squared  (gensym)
        gt-squared (gensym)]
    (-> system
        ((constant (make-interval 9.789 9.790)) g)
        ((constant 0.5) one-half)
        (quadratic t t-squared)
        (product g t-squared gt-squared)
        (product one-half gt-squared h))))

(defn similar-triangles
  [system s-ba h-ba s h]
  (let [ratio (gensym)]
    (-> system
        (product s-ba ratio h-ba)
        (product s ratio h))))
```

Likewise, when setting up the system, each function returns a new altered system that we need to thread through to the next function. We no longer need to bind any global variables - we simply pass our merge function to `make-system` when creating a blank system.

```clojure
(let [custom-merge (doto (default-merge) extend-merge)
      system (make-system custom-merge (default-contradictory?))
      result-system (-> system

                        (fall-duration :fall-time :building-height)
                        (add-value :fall-time (make-interval 2.9 3.1))

                        (similar-triangles :barometer-shadow :barometer-height
                                           :building-shadow :building-height)
                        (add-value :building-shadow (make-interval 54.9 55.1))
                        (add-value :barometer-height (make-interval 0.3 0.32))
                        (add-value :barometer-shadow (make-interval 0.36 0.37))

                        (add-value :building-height 45.0))]

  [(get-value result-system :building-height)
   (get-value result-system :building-shadow)
   (get-value result-system :barometer-height)
   (get-value result-system :barometer-shadow)
   (get-value result-system :fall-time)])
```

The result is the same as before.

```clojure
[45
 {:lo 54.9, :hi 55.1}
 {:lo 0.3, :hi 0.30327868852459017}
 {:lo 0.366, :hi 0.37}
 {:lo 3.025522031629098, :hi 3.0321598338046556}]
```

To re-iterate, the system approach requires us to feed the system to each call and use the altered system in future calls. This means that

- We no longer have to bind a global `*merge*` function.
- Steps are no longer performed in implicit `dosync`s.
- No propagators are put in global vars, they are kept in the system.
- We can take a system at any time, perform experiments on it and choose to use it, or to revert back to our old copy (or copies) of the system.

Have a go at altering facts and relations in the system. It helps in understanding which relations and values influence what in the system.
