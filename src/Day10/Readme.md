# Day 10

A parsing problem!  In general, errors in parsing are something that I try to get out in the first step, but in this problem the parsing will just be interpreting a few characters and then the bulk of the problem will be actually inferring the structure.

I opted to separate out the data structures for open/close and for the chunk type.  I noticed that scoring is just on the type of the chunk, so it seems that open and close is mostly just about positioning.  This bore out well in the second part since it really just cared about the missing types, since all of them would be closing characters.

An interesting wrinkle is that while I was writing part 1 I ended up with a case in my parser that I wasn't sure how to handle.  It was the case where we had run out of input but still had chunks to close.  I realized these were important to filter out as incomplete and tagged them as such, which made reusing it for part 2 very simple.

## Optimization

This problem is pretty staight forward so it's going to be hard to improve on the timing.  The parsing is actually the slowest part here, for some reason.  The program could probably be sped up significantly if the raw character string were used.  Though significant is only a millisecond or two.

```
Parsing input...
benchmarking...
time                 1.505 ms   (1.499 ms .. 1.512 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.519 ms   (1.510 ms .. 1.534 ms)
std dev              37.25 μs   (26.64 μs .. 50.84 μs)
variance introduced by outliers: 12% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 68.51 μs   (67.93 μs .. 68.93 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 67.53 μs   (67.30 μs .. 67.87 μs)
std dev              959.2 ns   (754.3 ns .. 1.178 μs)

Part 1: 392097
================================================================================
Running Part 2...
benchmarking...
time                 75.89 μs   (75.45 μs .. 76.53 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 76.78 μs   (76.30 μs .. 77.47 μs)
std dev              1.813 μs   (1.298 μs .. 2.441 μs)
variance introduced by outliers: 20% (moderately inflated)

Part 2: 4263222782
```