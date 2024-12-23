# aoc2024
Advent of Code 2024 in Fortran

## 1: Historian Hysteria
Sum the absolute differences between two lists.
Then identify how many times is item from list-1 present in list-2.

## 2: red-Nosed Reports
Verify that numbers at a single line are all incresasing or decreasing
and that the differences are within a specified limits. Part 2 requires
to verify if the conditions are met if one of the numbers is removed.
Easy solution, using brute-force for Part 2.

## 3: Mull It Over
Parsing exercise - find "mul(a,b)" and "do()" and "don't()" instruction.
Clumsy algorithm using "state-machine", a more general solution to
parsing could be made (if time permits)...

## 4: Ceres Search
On a 2D character map, count how many times a word "XMAS" can be found.
The loop over neighbours of a particular position on the map.

## 5: Print Queue
Verify if the order of numbers in an array meets the list of rules.
In part 2, reorder the numbers according to the rules.
Is it possible to make a faster algorithm for part 2?

## 6: Guard Gallivant
Make an agent walking through the map. Count the length of his path in Part 1
and look if path does not result in the loop in Part 2.
The most enjoyble/relaxed problem this year so far!

## 7: Bridge Repair
Nice problem for DWS. First implemented usin loop iterator to test all
combinations of operators (2^11 v. 3^11 in Parts 1 and 2).
Optimized by pruning the branches that alredy overshoot the required value,
run-time dropped from 142 s to 14 s.

## 8: Resonant Collinearity
In a 2D-map find (count) the number of points that are on the line between
nodes with the same id. Loop over all nodes pair.
Vector algebra, using "pp = aa + k*(bb-aa)". It took some work to set the
range for "k" so that "pp" stays within the map.

## 9: Disk Fragmenter
Move blocks on the disc. Using doubly-linked list, but some cleaning could
be done.

## 10: Hoof It
First path finding problem. Using simple recursion, eliminating duplicates
in Part 1, but keeping all paths in Part 2.

## 11: Plutonian Pebbles
Implement rules to mutate the row of stones with numbers on them.
They ask for the number of stones after certain number of steps.
The number of stones grows exponentially, however, the number of *unique*
stones is kept at low values (so we are able to make Part 2 by adding
the counter of particular stones).

## 12: Garden Groups
A nice topological problem. Part 1 - label connected components and calculate
their area and circumfence. Part 2 - calculate the number of sides instead of
the circumfence. My algorithm is to generate edges at all heterogeneous
contacts and then reduce the number of edges by merging them if the tail of
the first edge coincides with the head of the second one. The orientation
of edges is very important to avoid merging edges that would cross-each
other - see the last sample input.

## 13: Claw Contraption
Using linear algebra - solving a set of two linear equations

## 14: Restroom Redoubt
Simulate movement of robots in Part 1. Part 2 required to find a christmass
tree pattern. A vague description - could not solve first. The most universal
solution is to calculate the entropy - the sum of square distance among the
robots and find when this entropy drops to an minimum.

## 15: Warehouse Woes
Simulate moving the boxes in a warehouse. Very nice task. And an example of
an OOP.

## 16: Reindeer Maze
First Djikstra of the year. The trick is to use space (x,y,heading). There is a
cost for moving forward and also cost for rotating. Find the least costly path
from start to finish

## 17: Chronospatial Computer
Write an interpreter of a simple program in Part 1. Reverse engineer the code.
Remember what these operations do:
- dividing a number by 2**i (it shifts bits)
- modulo
- and XOR operation.
Try to find the starting number to be set to registers that program produces
a specific output.

## 18: RAM Run
Map where each second a new tile (#) appears. Find the shortest path. 
Used a simple Djikstra to solve.

## 19: Linen Layout
Find the number of ways a long pattern can be covered by shorter patterns from 
a list. Using a simple recursion works for Part 1 but the number grows
exponentially in Part 2. Storing the result to avoid repeated calculations.
Similar idea as optimizing Levensheim distance for example.

## 20: Race Condition
A single path labyrinth. Find the number of "cheats" that allow to ignore
labyrinth walls. Using Djikstra to assign the distance from the start to
each '.' tile. Then for each '.' tile exploring all other '.' tiles with
Manhattan distance less than 2 (or 20) to see if what type of shortcut is
possible. Nice problem.

## 21:

## 22: Monkey Market
Simulate pseudo-random number generator to obtain the sequence of 2000 values.
In part 2, store the differences in a tree (hight 4, 19 childrens), then
traverse the tree to find the sequence that gives the best sum of values.

## 23
Kerber-Brosh algorithm to find "cliques"
