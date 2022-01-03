# Day 25

It's another state iteration one.  This doesn't look like there are any tricks to reduce the area that needs to be considered without adding a lot of computation, so we can just try to brute force it.  Back to `STUArray`!

## Optimization

I'm not at all happy with how long this took to run.  I did a little profiling and the biggest offender is the memory allocation when comparing the arrays.  Apparently those lists are not lazy (makes sense when they are mutable) and there is no primitive comparison function!  I'll have to write my own some day or contribute back.

The simpler way to improve this is just have the updating function return whether anything has changed.  Not too hard to switch from `CucumberM s ()` to `CucumberM s Bool`.

```
Parsing input...
benchmarking...
time                 3.252 ms   (3.195 ms .. 3.333 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 3.269 ms   (3.240 ms .. 3.307 ms)
std dev              105.5 μs   (86.21 μs .. 131.0 μs)
variance introduced by outliers: 16% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 3.808 s    (3.711 s .. 3.929 s)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 3.739 s    (3.715 s .. 3.775 s)
std dev              34.12 ms   (1.407 ms .. 42.47 ms)
variance introduced by outliers: 19% (moderately inflated)

Part 1: 565
================================================================================
Running Part 2...
benchmarking...
time                 7.940 ns   (7.807 ns .. 8.048 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 7.924 ns   (7.824 ns .. 8.040 ns)
std dev              371.2 ps   (308.8 ps .. 463.0 ps)
variance introduced by outliers: 72% (severely inflated)

Part 2: 0
```
