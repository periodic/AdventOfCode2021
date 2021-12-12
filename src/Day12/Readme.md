# Day 12

Let's find some paths!  A few things stand out right away about this problem.  First, we are calculating all paths and can visit some nodes multiple times.  This means that there cannot be any of those nodes connected to each other otherwise we would get a cycle and the number of paths would be infinite.  In the language of the problem, no two big caves are directly connected.  Another is that the input is actually quite small, being very amenable to brute force, meaning there isn't a pressing need to find clever optimization.

Part two created a tricky wrinkle because you can only visit _one_ small cave twice.  I wasted plenty of time thinking it was all caves twice and tracking the number of times each cave was visited, but in reality it was just a little boolean I had to have tag along and flip.

## Optimization

I was a little worried that this problem would blow up exponentially and require tricks beyond brute force.  Fortunately, the inputs are small so that wasn't necessary.  It's possible we could get this runtime down even further, but I'd want to find ways to reduce the overall complexity as there isn't much issue at this scale.

It's amazing just how much slower part 2 is than part 1 with just a simple change.  That alone blows up the problem exponentially because suddenly each path can have many different additions at many different points depending on the degree of the nodes.

I did try representing the visited set as a list because it's small so it's possible that scanning would be faster, but with all the indirection for incremental lists it probably isn't worth it and it was about 80% slower.  It's possible that a sequence would work better.

```
Parsing input...
benchmarking...
time                 2.179 μs   (2.166 μs .. 2.195 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.168 μs   (2.160 μs .. 2.178 μs)
std dev              28.73 ns   (18.85 ns .. 42.26 ns)
variance introduced by outliers: 11% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 4.198 ms   (4.173 ms .. 4.223 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.224 ms   (4.215 ms .. 4.234 ms)
std dev              30.78 μs   (23.44 μs .. 43.81 μs)

Part 1: 3761
================================================================================
Running Part 2...
benchmarking...
time                 109.9 ms   (108.7 ms .. 110.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 109.5 ms   (109.1 ms .. 109.8 ms)
std dev              597.2 μs   (437.7 μs .. 815.0 μs)

Part 2: 99138
```