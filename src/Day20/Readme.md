# Day 20

Overall this algorithm is pretty straight-forward. We're just doing a lot of lookups in a table and updating values.  The only tricky part is the need to keep track of what value the "infinite" part of the image has, because that has an effect on how the image evolves.  They cleverly hide this during the example by making a value of 0 map to off, but when the infinite part can change the grid can expand three times as fast!

##  Optimization

I'm not too happy with the second part.  If it really were just 25 times as many iterations then it should take `25ms * 25 = 625ms`, but it took `1.75s`.  This is probably due to the expanding grid.

Possible avenues of exploration include:

- Use unboxed vectors/arrays to store data instead of maps.  I used an IntMap for the algorithm, but since it's size is fixed and it is immutable I could use a much more fixed datastructure.
- Use a better data type than Bool.  It seems to map well to the problem space for on/off, but may not store as efficiently as I'd like.
- Maybe there's a way to calculate the next iteration without reading each square nine times by keeping a running tally or something.

```
Parsing input...
benchmarking...
time                 770.5 μs   (763.4 μs .. 778.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 757.5 μs   (753.4 μs .. 763.5 μs)
std dev              16.71 μs   (10.96 μs .. 26.24 μs)
variance introduced by outliers: 12% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 24.83 ms   (24.29 ms .. 25.19 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 25.33 ms   (25.07 ms .. 25.98 ms)
std dev              842.0 μs   (442.0 μs .. 1.542 ms)

Part 1: 5464
================================================================================
Running Part 2...
benchmarking...
time                 1.748 s    (1.692 s .. 1.786 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.808 s    (1.782 s .. 1.858 s)
std dev              49.02 ms   (176.1 μs .. 59.48 ms)
variance introduced by outliers: 19% (moderately inflated)

Part 2: 19228
```