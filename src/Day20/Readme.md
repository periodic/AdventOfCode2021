# Day 20

Overall this algorithm is pretty straight-forward. We're just doing a lot of lookups in a table and updating values.  The only tricky part is the need to keep track of what value the "infinite" part of the image has, because that has an effect on how the image evolves.  They cleverly hide this during the example by making a value of 0 map to off, but when the infinite part can change the grid can expand three times as fast!

##  Optimization

I'm not too happy with the second part.  If it really were just 25 times as many iterations then it should take `25ms * 25 = 625ms`, but it took `1.75s`.  This is probably due to the expanding grid.

Possible avenues of exploration include:

- Use unboxed vectors/arrays to store data instead of maps.  I used an IntMap for the algorithm, but since it's size is fixed and it is immutable I could use a much more fixed datastructure.
  - Changing the algorithm map to an unboxed vector gave only a modest 5% speed up.
  - Over half the time is spent just looking up values!  Switching to an unboxed array for the image data, even if we rebuild it each time, brings execution time down by about 70%.
- Use a better data type than Bool.  It seems to map well to the problem space for on/off, but may not store as efficiently as I'd like.
- Maybe there's a way to calculate the next iteration without reading each square nine times by keeping a running tally or something.

```
Parsing input...
benchmarking...
time                 749.7 μs   (745.1 μs .. 755.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 764.8 μs   (758.9 μs .. 777.7 μs)
std dev              28.18 μs   (11.30 μs .. 45.99 μs)
variance introduced by outliers: 28% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 7.921 ms   (7.806 ms .. 8.069 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 7.820 ms   (7.771 ms .. 7.895 ms)
std dev              177.0 μs   (122.2 μs .. 244.0 μs)

Part 1: 5521
================================================================================
Running Part 2...
benchmarking...
time                 549.5 ms   (346.4 ms .. 703.6 ms)
                     0.985 R²   (0.945 R² .. 1.000 R²)
mean                 598.9 ms   (563.8 ms .. 626.1 ms)
std dev              34.11 ms   (17.91 ms .. 40.90 ms)
variance introduced by outliers: 19% (moderately inflated)

Part 2: 20247
```