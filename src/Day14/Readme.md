# Day 14

Some sequence rules!  This is a lot like the recurrence relations, but on text strings instead of numbers.  I wonder if this is related to regular languages.

I assumed that the second part of this problem would care about the order of the polymer, such as searching for substrings, so my initial solution was to use lists.  Lists are great for this sort of substitution because they are very easy to scan and insert with.  However, this wasn't the case and part two just required more iteration.

This brought out the idea to track just the counts of pairs, which turned out to be a relatively simple proposition. One thing I learned here is that `Data.Map.insertWith` puts the new value as the first element, so it works great for lists (`:`), but it not so great for subtraction without flipping it.

## Optimization

The performance wasn't bad.  I'm sure we could improve performance considerably by optimizing our map.  There are only four elements in the string so there are only 16 pairs possible.  This would easily accommodate a vector if we could make the pairs a member of `Ix`.

```
Parsing input...
benchmarking...
time                 10.37 μs   (10.34 μs .. 10.42 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.36 μs   (10.35 μs .. 10.39 μs)
std dev              67.91 ns   (50.42 ns .. 107.2 ns)

================================================================================
Running Part 1...
benchmarking...
time                 428.8 μs   (423.3 μs .. 437.6 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 429.6 μs   (427.4 μs .. 433.2 μs)
std dev              9.687 μs   (6.439 μs .. 14.39 μs)
variance introduced by outliers: 14% (moderately inflated)

Part 1: 2435
================================================================================
Running Part 2...
benchmarking...
time                 5.049 ms   (4.968 ms .. 5.131 ms)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 5.010 ms   (4.972 ms .. 5.060 ms)
std dev              137.0 μs   (102.8 μs .. 200.6 μs)
variance introduced by outliers: 11% (moderately inflated)

Part 2: 2587447599164
```