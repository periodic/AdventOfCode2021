# Day 18

This problem looks like a perfect use-case for [zippers](https://wiki.haskell.org/Zipper).  Zippers let us move around in a data structure and do targeted updates in a purely functional way without having to constantly recreate the structure or find our position in it.

One thing that makes these operations odd is that they care about the order of the numbers and the tree structure.  That means there is a lot of tree walking that adds to the code.

In the end, I think the code works pretty well, but I think it could be simpler.  I implemented both a zipper and a monad for operations on the structure. This solution is pretty clean in the way that it split the logic up, but there were a few more error cases to deal with than I'd like.

I did get to play around with zippers and the `Except` monad!

## Optimization

The zipper structures are likely leading a lot of extra allocations.  This is something that a mutable language with a doubly-linked tree wouldn't have trouble with because they would just have one pointer for the location instead of a list.

Overall the performance is snappy enough it won't make my top list for rewriting.

```
Parsing input...
benchmarking...
time                 68.24 μs   (67.74 μs .. 68.87 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 67.82 μs   (67.69 μs .. 68.06 μs)
std dev              587.0 ns   (364.2 ns .. 1.048 μs)

================================================================================
Running Part 1...
benchmarking...
time                 7.363 ms   (7.208 ms .. 7.510 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 7.227 ms   (7.188 ms .. 7.286 ms)
std dev              137.0 μs   (96.50 μs .. 173.0 μs)

Part 1: 2907
================================================================================
Running Part 2...
benchmarking...
time                 94.53 ms   (93.75 ms .. 95.37 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 94.36 ms   (94.03 ms .. 95.04 ms)
std dev              726.8 μs   (315.1 μs .. 1.115 ms)

Part 2: 4690
```