# Day 19

Looks like return of the dragons!  Last year there was a similar problem about arranging and connecting fragments into a greater whole.  This one was no different and took an order of magnitude longer than the previous ones. There is just something about these that I just don't seem to get.

In the end, I ended up with a very brute-force solution.  There may be a clever way to do this quickly, but I just wanted to get it done.  I'm not proud of this solution, but it works.

I made a few false starts that may be visible in the code still.  I made it more complicated than it needed to be.  I probably just wanted to track a rotation matrix and an offset for each one and do a little more linear algebra.

## Optimization

This one is a doozy.  I'm not happy with the run-time at all.  In the future I'd lean on more linear algebra.

There should also hopefully be a way to do this without just trying every combination.  One possible heuristic is to calculate the nearest neighbor numbers for each point in a scanner's set.  That would give a rotation and position independent way to compare them.

```
Parsing input...
Work took 0.578ms
================================================================================
Running Part 1...
Work took 77286.732ms
Part 1: 372
================================================================================
Running Part 2...
Work took 77148.417ms
Part 2: 12241
```