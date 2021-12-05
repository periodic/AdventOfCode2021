# Day 5

https://adventofcode.com/2021/day/5

## Part 1

This first half looks pretty straightforward.  There are a lot of potential complications, but most of them are excluded by the problem definition.  For example, what if the start and end of an input line doesn't describe a horizontal or vertical line?

The key insight here is that we need to be able to look up from points to counts, so this is going to be some sort of mapping data structure.  It looks like it may very well be a sparse map, so a `Map` will likely do fine.  It's also very easy to work with and it'll be trivial to compute the total number of points satisfying a condition (where those points have been defined).  Note also that the bounds aren't specified, so this will make it easy to define incrementally.

I also don't see a need to do any vector math yet, so I'll just use tuples for the coordinates instead of `V2`.

I ran into two major mistakes with this part.  First, I assumed that all the vents would be moving in positive directions, which isn't true and should be obvious from inspecting the example input.  Second, I wrote up a general solution that included diagonal vents thinking that the problem was telling me that's all I had to consider in the input and not realizing I actually had to filter the input.

## Part 2

I unwittingly solved part 2 while doing part 1 because I made sure to accommodate diagonal vents.  Implementing part 2 was just a matter of doing less work by not filtering.

## Optimization

Initial solutions take on the order of milliseconds to run, but this could probably come down into the microseconds with a little work.

TODO:

- A mutable unboxed array is probably going to be much faster to create and scan.
- Is building lists of coords for the vents slower than iteratively calculating them?

```
Parsing input...        
benchmarking...
time                 109.9 μs   (109.3 μs .. 110.7 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 110.4 μs   (109.9 μs .. 112.2 μs)
std dev              2.777 μs   (796.3 ns .. 5.712 μs)
variance introduced by outliers: 21% (moderately inflated)

Number of vents: 500
================================================================================
Running Part 1...
benchmarking...
time                 82.24 ms   (77.05 ms .. 86.55 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 85.73 ms   (84.34 ms .. 87.60 ms)
std dev              2.757 ms   (2.023 ms .. 3.477 ms)

Number of points with overlapping vents: 7142
================================================================================
Running Part 2...
benchmarking...
time                 185.4 ms   (176.0 ms .. 193.3 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 192.0 ms   (187.7 ms .. 196.8 ms)
std dev              6.613 ms   (3.386 ms .. 10.68 ms)
variance introduced by outliers: 14% (moderately inflated)

Number of points with overlapping vents: 20012
```