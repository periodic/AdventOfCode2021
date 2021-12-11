# Day 9

The first part of the problem is pretty straight forward.  We can read all the data into a map and then just scan over the map looking for values less than all the neighbors and put that into a sum.  The Haskell types work great here with out-of-bounds values using `Maybe` and being easy to filter out with `catMaybes` or using `all` with a function that treats `Nothing` as `True`.

The second part is a problem I've known as "islands".  The simplest solution to this is to keep track of what's visited and basically just check each coordinate and flood-fill from there.  There are lots of additional tricks you can do, but it gets the job done.  I chose to do this with some Monads because it just feels silly to be passing a bunch of state and config around.

## Optimization

```
Parsing input...
benchmarking...
time                 4.161 ms   (4.146 ms .. 4.184 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.213 ms   (4.200 ms .. 4.230 ms)
std dev              45.68 μs   (37.97 μs .. 54.92 μs)

================================================================================
Running Part 1...
benchmarking...
time                 2.379 ms   (2.369 ms .. 2.394 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.377 ms   (2.370 ms .. 2.396 ms)
std dev              36.73 μs   (12.01 μs .. 70.79 μs)

Part 1: 516
================================================================================
Running Part 2...
benchmarking...
time                 12.44 ms   (12.39 ms .. 12.50 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.43 ms   (12.38 ms .. 12.49 ms)
std dev              141.9 μs   (101.6 μs .. 201.3 μs)

Part 2: 1023660
```