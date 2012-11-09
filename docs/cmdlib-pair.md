# Pair Commands

A pair is a Scripty construct `'key=value'`. These commands let you manipulate this data structure. A Pair is an immutable parser artifact. There are only test functions and getter functions.

## Reference

**pair?**

Test whether an object is of type pair.

	(pair? obj)

**pair-left**

Get the left part of a pair.

	> print (pair-left key=value)
	key

**pair-right**

Get the right part of a pair.

	> print (pair-right key=value)
	value	