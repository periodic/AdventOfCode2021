# Day 15

This question looks all about pathfinding!  The first part is a great application of the wonderful A* algorithm. 

I must have mis-remembered this algorithm because it took way longer than I expected to implement.  It also took much longer to run!

## Optimization

There is a lot of room for optimization here, particularly around the use of maps, queues and sets.  Particularly I expect that the lookup could be made purely programmatic instead of generating a new Map.  Just generating the map for part 2 took ~82ms which is longer than part 1 at ~51ms.  That shouldn't take so long.

In the future I'll dig in and clean this up.

```
Parsing input...
benchmarking...
time                 3.893 ms   (3.878 ms .. 3.904 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.919 ms   (3.905 ms .. 3.937 ms)
std dev              51.08 μs   (35.25 μs .. 70.84 μs)

================================================================================
Running Part 1...
benchmarking...
time                 38.09 ms   (37.71 ms .. 38.52 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 37.99 ms   (37.80 ms .. 38.21 ms)
std dev              414.9 μs   (322.6 μs .. 590.7 μs)

Part 1: 361
================================================================================
Running Part 2...
benchmarking...
time                 1.683 s    (1.426 s .. 1.870 s)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 1.718 s    (1.685 s .. 1.743 s)
std dev              35.05 ms   (18.14 ms .. 48.41 ms)
variance introduced by outliers: 19% (moderately inflated)

Part 2: 2838
```