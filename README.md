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