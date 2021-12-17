# Day 17

Mechanical simulation!

This problem looks a lot like all those physics problems you would get in introductory mechanics, but it's not quite.  First, there is drag.  Second, it's discrete, so many of our classical equations aren't quite right.  However, that doesn't mean we can't solve part 1 with a little math.

In part 1 we are looking for the maximum height we can reach.  Note that the two axes, horizontal and vertical, are independent.  They don't depend on each other.  Thank goodness there are no cross currents at different depths!

Now observe that the maximum height will likely be reached in a situation where the horizontal velocity becomes zero, because then we can make our height as high as we want it without worrying about overshooting in the horizontal.  The total distance that the probe will travel horizontally is `v_x0 + (v_x0 - 1) + ... + 2 + 1`.  This is just a triangle sum and the total is `v_x0 * (v_x0 + 1) / 2`, so if a value exists that lands in the target area then we know we will not be constrained on the number of iterations.  It appears that the input is crafted for this (at least in my case).  The actual `v_x0` does not matter.

Once we know that we have all the time in the world to play with, we just need to find the maximum height.  A useful observation is that the heights that the probe registers at on the way up are the same on the way down, due to how it always changes by only one each step.  This means that the probe will end up at a height of 0 on the way down.  The speed is also the same at that step as it was on the the way up, just with the direction reversed.

Also, it appears that all the target areas are below the origin line.  This may not be true for everyone, but it certainly was for me, so I'm assuming it's part of the problem.  It would just need a bit more calculation if that wasn't the case.

So how do we find the maximum height?  We want the maximum velocity that will put us in the target area in a single step without overshooting.  The higher that velocity, the higher the corresponding initial velocity and the higher the probe will have gone.  That will correspond to an initial velocity of `target_ymin + 1`.  Remember, the target number is negative so it's min instead of max and plus insteaad of minus. The height of the probe is another triangle number, just like for the horizontal, so the height will be `v_y0 * (v_y0 + 1) / 2`.

At least in my case I calculated this and it worked out great!