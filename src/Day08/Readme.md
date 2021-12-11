# Day 8

This seems like a simple constraint-solving problem.  Granted, it can be solved very easily analytically because there is no need for backtracking.  In fact, each piece of the display can be determined by a combination of unions, intersections and differences on the sets of active segments.

I didn't want to think about this too hard, so I wrote something that did a little constraint solving.  This proved to be ill-advised because it ended up a book-keeping nightmare.

Another insight would be that the actual mapping is entirely unnecessary.  As long as you can figure out which sequences correspond to which number you can figure out which digits are on display.

That remains for a future optimization.