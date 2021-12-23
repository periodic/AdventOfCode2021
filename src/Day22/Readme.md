# Day 22

The first part of this problem gave a pretty clear hint as to what the second part would be.  Restricting the input gives away that unrestricting the input is a likely path.  I don't think I've ever seen one of these problems waste input.

The quick and obvious way to solve this is to use a `Set` of the on points and use `Ix.range` to generate additional `Set`s that have to be combined using `union` and `difference`.  That works fine for inputs in the `[-50,50]` range, but will quickly use up too much memory as it grows cubicly with the problem space.

Instead we want something that scales with the input size.  We can represent entire regions at once to take up a lot less space!  Again, we only have to represent the regions that are on.

The trick comes in calculating a way to represent the remaining space when only part of a region is turned off.  This has a bunch of fiddly math because subtracting on region from another can result in no less than six rectangular regions if the subtracted space is in the middle.  Indeed, this was where I spent most of my debugging.

Once we have that, the difference between the two parts is a pretty trivial restriction of the space where we just go through and filter the input rules or rewrite them to force them into the area we care about.

## Optimization

My first implementation put all the regions in a list because that was quick and easy.  This has a drawback of having to scan the whole list every time we want to do intersections.  This could be made more efficient with some sort of [R-Tree](https://en.wikipedia.org/wiki/R-tree) so that we can quickly look up the intersecting regions without scanning.  This could reduce the algorithm from `O(n²)` to `O(n log n)` on the input size.  However, there's a hidden cost: processing the intersections is far more expensive than calculating overlap.  If we are quickly skipping non-overlapping regions then the scan is not going to be very expensive and the real complexity is `O(n * avg_n_overlap)`.

```
Parsing input...
benchmarking...
time                 449.1 μs   (446.3 μs .. 453.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 449.3 μs   (446.3 μs .. 454.4 μs)
std dev              12.40 μs   (7.725 μs .. 18.07 μs)
variance introduced by outliers: 20% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 91.90 μs   (91.11 μs .. 92.74 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 91.28 μs   (90.90 μs .. 91.84 μs)
std dev              1.548 μs   (1.130 μs .. 2.036 μs)
variance introduced by outliers: 11% (moderately inflated)

Part 1: 588120
================================================================================
Running Part 2...
benchmarking...
time                 14.77 ms   (14.34 ms .. 15.25 ms)
                     0.992 R²   (0.984 R² .. 0.997 R²)
mean                 15.23 ms   (14.91 ms .. 15.61 ms)
std dev              859.4 μs   (714.5 μs .. 1.050 ms)
variance introduced by outliers: 23% (moderately inflated)
```
