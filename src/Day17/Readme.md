# Day 17

Mechanical simulation!

This problem looks a lot like all those physics problems you would get in introductory mechanics, but it's not quite.  First, there is drag.  Second, it's discrete, so many of our classical equations aren't quite right.  However, that doesn't mean we can't solve part 1 with a little math.

In part 1 we are looking for the maximum height we can reach.  Note that the two axes, horizontal and vertical, are independent.  They don't depend on each other.  Thank goodness there are no cross currents at different depths!

Now observe that the maximum height will likely be reached in a situation where the horizontal velocity becomes zero, because then we can make our height as high as we want it without worrying about overshooting in the horizontal.  The total distance that the probe will travel horizontally is `v_x0 + (v_x0 - 1) + ... + 2 + 1`.  This is just a triangle sum and the total is `v_x0 * (v_x0 + 1) / 2`, so if a value exists that lands in the target area then we know we will not be constrained on the number of iterations.  It appears that the input is crafted for this (at least in my case).  The actual `v_x0` does not matter.

Once we know that we have all the time in the world to play with, we just need to find the maximum height.  A useful observation is that the heights that the probe registers at on the way up are the same on the way down, due to how it always changes by only one each step.  This means that the probe will end up at a height of 0 on the way down.  The speed is also the same at that step as it was on the the way up, just with the direction reversed.

Also, it appears that all the target areas are below the origin line.  This may not be true for everyone, but it certainly was for me, so I'm assuming it's part of the problem.  It would just need a bit more calculation if that wasn't the case.

So how do we find the maximum height?  We want the maximum velocity that will put us in the target area in a single step without overshooting.  The higher that velocity, the higher the corresponding initial velocity and the higher the probe will have gone.  That will correspond to an initial velocity of `target_ymin + 1`.  Remember, the target number is negative so it's min instead of max and plus insteaad of minus. The height of the probe is another triangle number, just like for the horizontal, so the height will be `v_y0 * (v_y0 + 1) / 2`.

At least in my case I calculated this and it worked out great!

Part 2 is just a bit more fiddly.  I took the approach of calculating the iterations on which different `v_y` values intersect the target area.  I put these in a map keyed on which iterations they are valid for.  That way I just have to iterate a bunch of `v_x` values and can pull the `v_y` values that they need to pair with based on what iterations they are crossing the target area.  Don't forget to deduplicate because one pair may cross the target area twice!

## Optimization

I was afraid that this problem would take too long to run.  For large inputs that is definitely a concern.  Given that numbers are on the order of 200 we only have to try about 40,000 distinct pairs even if we brute forced it.  With the algoritm above it's much closer to `max_y * (max_x - min_x)` because we do Xs and Ys separately and have only to look up a subset of Xs.  It's more complicated because the map has logarithmic insertion, but the point is that it's less than the full product.

Anyway, the runtime is small enough that I don't think it's worth optimizing at this point.

```
Parsing input...
benchmarking...
time                 417.3 ns   (416.1 ns .. 418.7 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 415.5 ns   (414.2 ns .. 417.8 ns)
std dev              5.652 ns   (3.913 ns .. 9.222 ns)
variance introduced by outliers: 13% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 6.706 ns   (6.675 ns .. 6.745 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.791 ns   (6.749 ns .. 6.841 ns)
std dev              157.8 ps   (141.3 ps .. 176.1 ps)
variance introduced by outliers: 38% (moderately inflated)

Part 1: 7626
================================================================================
Running Part 2...
benchmarking...
time                 1.042 ms   (1.036 ms .. 1.049 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.050 ms   (1.045 ms .. 1.057 ms)
std dev              19.42 μs   (16.07 μs .. 22.88 μs)

Part 2: 2032
```