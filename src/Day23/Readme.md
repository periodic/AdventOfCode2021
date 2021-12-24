# Day 23

This one looks like another graph search problem, a lot like [Day 15](/src/Day15).  However, it's no longer a grid, but rather an abstract grid of states.  The really complicated part of this problem is representing the states and the transitions between them.

The state works well as a map from locations to amphipods.  This lets us do our most common operation which is looking up whether a amphipod is in a given spot.  This helps with looking at whether a path is blocked and where in a room the amphipod should sit.  An alternative would be to number the amphipod and track where they are, but that loses the fact that the amphipods are indistinguishable within a type.

Building the graph of possible moves is another complication.  Thankfully, amphipods can only move from a room to a hall or hall to a room, so there aren't too many options to consider.  This prevents us from having to deal with an Amber amphipod dancing in the hall because making a move for any other amphipod would be expensive and so the system isn't considering it.

Once we have those two parts, which describe the nodes and edges of the graph we can just do a search.  We can either do Dijiksta's algorithm or A*.  A heuristic isn't too hard to come up with, so I went with A*.  I used the cost of moving each amphipod into a room of the right type without considering any of the other amphipod.  As a lower-bound this works well and it'll help focus us on moving the high-costed amphipod more often.

## Optimization

This problem suffers all the problems Day 15 does, but we can't try any clever tricks that exploit the shape of the grit.

Things to try include:
- Data structure optimization
- Reduce the number of extra tuples that I create
- Remove debugging tools like the move log

```
Parsing input...
benchmarking...
time                 1.235 μs   (1.232 μs .. 1.239 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.239 μs   (1.236 μs .. 1.242 μs)
std dev              9.717 ns   (7.417 ns .. 13.82 ns)

================================================================================
Running Part 1...
benchmarking...
time                 137.8 ms   (135.6 ms .. 140.7 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 138.6 ms   (137.7 ms .. 139.9 ms)
std dev              1.709 ms   (1.054 ms .. 2.283 ms)
variance introduced by outliers: 11% (moderately inflated)

Part 1: 16508
================================================================================
Running Part 2...
benchmarking...
time                 2.011 s    (1.971 s .. 2.067 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.014 s    (2.002 s .. 2.030 s)
std dev              15.34 ms   (847.0 μs .. 18.94 ms)
variance introduced by outliers: 19% (moderately inflated)

Part 2: 43626
```