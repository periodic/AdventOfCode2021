# Day 6

## Background

This problem is one of my favorite classes of problems: a linear recurrence.  It's just like working with the Fibonacci Sequence or similar problems.  I find these fascinating because there are so many different ways to approach them and different algorithms have different trade offs.

There are two common approaches, starting from the top and starting from the bottom.

Starting from the top means defining a function like `f(n) = f(n - 1) + f(n - 2)`. The naive solution that is just based on the definition often ends up being exponential.  The value `f(t)` depends on various values from earlier `t` which depend on values before that.  If it depends on more than one `f(t)` then you can end up inefficiently calculating them multiple times.

The quick optimization is then to memoize.  This solves the inefficiency problem with a trade off of memory usage.

Instead you can start from the bottom and iteratively advance the problem, remembering values from the previous round.  This leads to a simple function like `f prev = calculate_next prev` which can be iterated the desired number of times.  This ends up nicely linear without the memory usage of the top-down solution.

Further, if the combination is strictly linear, then this can be done with linear algebra.  You can write a matrix, `A` such that `step(x) = A x`.  The key observation there is that `f(n) = A^n x` and exponentiation can be done using repeated squaring such that it only takes logarithmic time!  It's also great to be able to use highly efficient linear algebra libraries.

Finally, as a linear recurrence these problems actually have a closed-form solution that can be derived from the linear algebra, but it's very tedious to do.

## The Solution

I decided to do this with the linear algebra solution.  I haven't used much linear algebra in Haskell so this was going to be a good learning experience.
I was a little disappoint to find out there wasn't a matrix exponentiation function that I could find, but otherwise it was quite straight forward.

## Optimization

My first pass used linear squaring just so that I could get it done fast.  This lead to some decent but unimpressive runtimes of 44??0.08 ??s on part 1 and 114??0.25 ??s on part 2.  I then switched to using repeated squaring instead and that dropped the runtimes to <18??s each.

```
Parsing input...
benchmarking...
time                 10.92 ??s   (10.91 ??s .. 10.94 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 10.95 ??s   (10.94 ??s .. 10.97 ??s)
std dev              49.24 ns   (42.80 ns .. 56.17 ns)

================================================================================
Running Part 1...
benchmarking...
time                 17.46 ??s   (17.44 ??s .. 17.48 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 17.37 ??s   (17.33 ??s .. 17.41 ??s)
std dev              135.2 ns   (114.8 ns .. 159.6 ns)

Number of fish: 383160
================================================================================
Running Part 2...
benchmarking...
time                 17.65 ??s   (17.64 ??s .. 17.67 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 17.65 ??s   (17.63 ??s .. 17.67 ??s)
std dev              61.23 ns   (49.30 ns .. 77.47 ns)

Number of fish: 1721148811504
```