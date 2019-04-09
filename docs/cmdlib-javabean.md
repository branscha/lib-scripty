#### JavaBean Library

Access JavaBeans as a nested directory structure. This command library provides a number of commands to manipulate the JavaBeans in the context. It has a Naked Objects flavor. There is a current JavaBean like there is a current directory, it tries to apply the same traversal logic from file systems to JavaBeans. Obviously you need to put the JavaBeans in the context using your own custom commands.

**bean-cd**

Go to another location. Absolute or relative paths allowed.

* '/' is the context of the repl.
* '.' and '..' have the usual meaning.
* '[5]' indexing. Arrays, lists can be indexed. 'xyz[5]' has same semantics as 'xyz/5'.

**bean-pwd**

Print the current path to the repl.

**bean-ls**

List the contents of the denoted object. The action depends on the type of the object, properties, array elements are listed.

**bean-cat**

Convert the denoted object to a string and print it on the repl. The difference between 'ls' and 'cat' is that 'ls' lists the sub elements while 'cat' calls the toString() method.

**bean-rslv**

Convert a pathname to the object itself. We can obtain a direct reference to the object in this way. It is what the other commands do automatically. This command is in fact the link between other command libraries and this one.

**bean-call**

Call java methods on an object denoted by a path. The target object should be denoted by a pathname. The method can be specified by a method instance or by a name (and a lookup will occur). The arguments are not resolved, they are passed directly to the method. You can use the 'rslv' command above to accomplish argument resolution as well. It has the form (bean-call $bean method arg1 ... argN).

**bean-upd**

Set the property of a JavaBean. The argument should be a pair, a propname=value combination.
