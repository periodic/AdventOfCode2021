# Day 16

Oh boy, bit interpretation.  This always ends up very fiddly because being off by one can drastically change the program.  Something I found out myself!

I opted not to put much in my parsing step to be as true to doing work in the parts as possible.  So I wanted it to just treat the data as hexadecimal data, so that was pretty easy.

One thing I chose to do at this point was wrap up the strings in some `newtype`s just to make sure I didn't get them mixed up.  It's good practice and didn't create that much more work for me.

The actual packet parsing is a basic monadic parser, but without any backtracking or anything like that.  It's just a state monad that keeps track of remaining input and the current position.  I thought the current position would be important for dropping any padding, but padding doesn't occur in the middle of the packet so it can just be ignored.  It was useful for calculating when we had consumed enough packets, but really that could have been done with locals or something like that.

I ended up wasting about an hour because my hex-to-binary function was incorrect.  I accepted what GitHub Copilot gave me, but there was a typo where `A -> "1011"`.  `A = 10 = 0b1010`.  This kept giving me strange parses on some inputs and took me a lot of staring at bits to figure out.

Once I fixed that bug and my parsing actually worked for all inputs, part 
2 took less than five minutes, since it's a basic interpreter which is so easy to write with algebraic data types.

## Optimization

After working through this exercise I realized that a list of bits really is a good representation for this, that or something with cheap views.  The most common operation is splitting.  `Text` does this in linear time.  `Seq` does it in logarithmic time.  However, making views of a sequence that won't be modified should be constant time.

It's unclear if the input is actually big enough for this to matter.

```
Parsing input...
benchmarking...
time                 5.894 μs   (5.851 μs .. 5.954 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.915 μs   (5.887 μs .. 5.949 μs)
std dev              110.4 ns   (88.66 ns .. 175.5 ns)
variance introduced by outliers: 18% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 1.000 ms   (997.3 μs .. 1.004 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.001 ms   (998.5 μs .. 1.007 ms)
std dev              12.08 μs   (7.690 μs .. 20.85 μs)

Part 1: 969
================================================================================
Running Part 2...
benchmarking...
time                 1.042 ms   (1.041 ms .. 1.044 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.041 ms   (1.040 ms .. 1.045 ms)
std dev              7.760 μs   (3.571 μs .. 12.91 μs)

Part 2: 124921618408
```