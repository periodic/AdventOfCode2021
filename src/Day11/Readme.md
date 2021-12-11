# Day 11

At last, we get a state iteration problem!  There are always a handful of these.  This one is interesting because the grid is so small, so it's possible we won't get great gains by using something like a `STUArray`, but I'm going to do that anyway.

The second part of these questions is usually one of two complications.  Either run a longer amount of time or make a slight change to the rules.  In this case it's actually to run until a condition is met.  A few quick additions and we're in business!

## Optimization

I started out with an `STUArray`, which is a mutable unboxed array.  I'd be curious to check if a `Data.Map` is comparable or even faster for small amounts of data like this.  TThe size of the data stored might also matter, so using `Word8` instead of `Int` might help reduce the overhead.

The parsing on this one is very low.  I'd like to think that we could get part 2 to under a millisecond.

```
Parsing input...
benchmarking...
time                 12.94 μs   (12.87 μs .. 13.04 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 12.97 μs   (12.91 μs .. 13.07 μs)
std dev              251.2 ns   (116.9 ns .. 407.5 ns)
variance introduced by outliers: 18% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 472.5 μs   (460.3 μs .. 486.8 μs)
                     0.993 R²   (0.986 R² .. 0.999 R²)
mean                 451.9 μs   (445.6 μs .. 462.9 μs)
std dev              27.41 μs   (14.99 μs .. 42.80 μs)
variance introduced by outliers: 54% (severely inflated)

Part 1: 1694
================================================================================
Running Part 2...
benchmarking...
time                 1.218 ms   (1.212 ms .. 1.223 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.229 ms   (1.223 ms .. 1.242 ms)
std dev              28.99 μs   (12.01 μs .. 44.99 μs)
variance introduced by outliers: 13% (moderately inflated)

Part 2: 346
```