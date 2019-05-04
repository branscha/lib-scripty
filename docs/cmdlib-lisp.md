# List Commands

This module provides the basic Data manipulation commands, they are not 
built-in but provided as a separate command library. The semantics deviates from Common Lisp because we based our lists 
on Java lists and not on cons constructs. Consing for example modifies the existing list, while in Lisp it creates a 
new version, the original binding refers to the sublist. This can lead to surprising results if one is used to the 
original Lisp semantics.

**list?**

A test to see if the argument is a list or not. The other command types are only applicable if this test turns out positive.

**empty?**

A test to see if the list is empty.

**member?**

A test to see if an element is part of the list or not. (member? <list> <el>)

**car**

The first element of the list.

**cdr**

A copy of the list without the first element. Non destructible on the original.

**pop**

Get the element at the end of the list. Modifies the list and returns the element.

**shift**

Get the first element of the list and modifies the list. It returns the element.

**push**

Add one or more elements at the end of the list. Modifies the list and returns the list. (push <list> el1 ... eln)

**unshift**

Insert one or more elements at the beginning of the list. (unshift <list> el1 ... eln)

**cons**

Insert one element at the beginning of the list. The list is modified, destructible on the original.(cons el <list>)

**append**

Append two or more lists into a single new list. Non destructible.

**size**

The number of elements in the list. The result is a string representing an integer.

**dup**

Make a shallow copy of the list.

**null?**

A test to see if the argument is null.

