# Day 7

What I notice about this problem immediately is that it's less about algorithms and more about statistics/fits.  Part 1 is a linear median.  Part 2 is a linear least-squares fit.  The heart of the problem is minimizing a function on the range of horizontal measurements.

Part 1 is pretty simple.  Just find the median, then sum the absolute differences from that to find the total fuel cost.  Fortunately it's simplified by knowing that we don't have to handle the case where the median lies between two values.

Part 2 is a little more complicated.  The simplest way to calculate a least-squares fit is to try all the values and find the one that is smallest. We could use some gradient descent or some fancier algorithms, but simply trying them all is very reasonable given our input.