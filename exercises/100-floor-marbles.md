## Exercise 2-46

You have a 100-story building and a couple of marbles. You must identify the lowest floor for which a marble will break if you drop it from this floor. How fast can you find this floor if you are given an infinite supply of marbles? What if you only have two marbles?

### Answer
If you have an infinite supply of marbles, you could use 'binary search' to find the lowest floor where the marble breaks. Start at floor 50, if the marble breaks, move to floor 25, otherwise move to floor 75 and try again. If the marble does not break, save that floor number. Repeat until there are no more floors to binary search, and return the value stored in your 'minimum floor' variable. This would run in lg(n) time.

If you only have two marbles, use the same algorithm, except you will only be able to get a rough approximation for the exact floor. Specifically, you will be able to narrow the range of possible floors to one quarter, or 25 floors.

It is worth noting that in practicality, for searching just 100 floors, it might be fastest to just use a sequential search, which starts at floor 0, and tries every floor until the marble breaks.