# Day 21

There has to be a few games to play at some point in the Advent of Code!  This one was a fun one, even though I made some bad decisions at first.

Part 1 is pretty simple.  It's just iterating on some state.  There might be a more direct way to do this, but it was quick and easy to implement.  It was clear that the type of die would be very important so I made sure to abstract that behind a class.

Little did I know that the die would become non-deterministic!  I did a few changes and quickly changed my monad to be a list monad modeling non-determinism, but this was far, far too slow because it doesn't remember things it's already done.

I used a fun trick for memoization which is to build a lazy map where the value of various entries are based on other entries.  Then with a few seed entries you can just look up the one you want and it will compute just what it needs for it.  This turned out to be nice and fast.

TODO: This solution needs some clean-up.

## Optimization

I'm still not sure if I'm cheating and laziness is allocating some things outside my functions, but it's nice and fast as is.  It's possible this could be done in a more efficient way so that it is fast,r but there is little need.

However, the memoization breaks my benchmark because it is only calculating the memoized values once.  I didn't want to go through hoops to get it to recompute it because it involves some mutually recursive functions.  You can just trust mi that it takes about 234ms.

```
Parsing input...
benchmarking...
time                 216.6 ns   (214.2 ns .. 219.3 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 216.8 ns   (214.9 ns .. 219.5 ns)
std dev              7.874 ns   (6.046 ns .. 9.815 ns)
variance introduced by outliers: 54% (severely inflated)

================================================================================
Running Part 1...
benchmarking...
time                 194.8 μs   (194.0 μs .. 195.9 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 192.4 μs   (191.3 μs .. 194.4 μs)
std dev              4.913 μs   (2.948 μs .. 8.685 μs)
variance introduced by outliers: 20% (moderately inflated)

Part 1: 1196172
================================================================================
Running Part 2...
benchmarking...
time                 117.5 ns   (114.7 ns .. 120.7 ns)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 115.8 ns   (115.1 ns .. 118.0 ns)
std dev              3.875 ns   (1.569 ns .. 7.606 ns)
variance introduced by outliers: 51% (severely inflated)

Part 2: 106768284484217
```