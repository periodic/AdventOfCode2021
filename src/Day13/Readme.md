# Day 13

When I first saw this question I was worried it was going to be similar to the dreaded dragons of last year.  Thankfully it seems much simpler than that.  It's just a few simple tranformations on the dots!  These are all just some simple mathematical reflections.  We don't even need to do anything fancy with mapping the dots to anything.

The second part had a huge twist.  The solution isn't a number!  In fact, this is the first problem I can think of that has a non-numeric answer.  I had to shift out of my usual scaffolding just so I could return a non-integer answer.

## Optimization

I'm not sure there is much to optimize here.  The sets are pretty efficient so the whole thing takes less than a millisecond for each part.  It's possible I could improve the text building by not using string concatenation.

```
Parsing input...
benchmarking...
time                 72.21 μs   (71.73 μs .. 72.86 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 73.37 μs   (72.42 μs .. 75.40 μs)
std dev              4.386 μs   (2.435 μs .. 8.210 μs)
variance introduced by outliers: 62% (severely inflated)

================================================================================
Running Part 1...
benchmarking...
time                 74.96 μs   (74.21 μs .. 76.05 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 75.10 μs   (74.51 μs .. 76.20 μs)
std dev              2.539 μs   (1.844 μs .. 4.128 μs)
variance introduced by outliers: 34% (moderately inflated)

Num. Dots: 610
================================================================================
Running Part 2...
benchmarking...
time                 506.2 μs   (504.7 μs .. 508.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 506.2 μs   (505.0 μs .. 507.7 μs)
std dev              4.457 μs   (3.666 μs .. 6.076 μs)

###..####.####...##.#..#.###..####.####
#..#....#.#.......#.#..#.#..#.#.......#
#..#...#..###.....#.####.#..#.###....#.
###...#...#.......#.#..#.###..#.....#..
#....#....#....#..#.#..#.#.#..#....#...
#....####.#.....##..#..#.#..#.#....####
```