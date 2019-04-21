#### JavaBean Library

Access JavaBeans as a nested directory structure. This command library provides a number of commands to manipulate the 
JavaBeans in the context. It has a Naked Objects flavor. There is a current JavaBean like there is a current directory, 
it tries to apply the same traversal logic from file systems to JavaBeans.

The library can also be useful while debugging a script that uses Java objects in it expressions. 
The library allows you to inspect these instances.

**bean-cd**

Go to another location. Absolute or relative paths allowed.

* '/' is the context of the repl.
* '.' and '..' have the usual meaning: the current location and the parent location.
* '[5]' indexing. Arrays, lists can be indexed. 'xyz[5]' has same semantics as 'xyz/5'.

**bean-pwd**

Print the current path, the result is a string.

**bean-ls**

List the contents of the denoted object. The action depends on the type of the object, properties, array elements are listed. 
By default the listing is printed on the output and returned as the result,  but the printing can be suppressed with the 
`quiet=true` option.

* quiet = true | false*. Prevents writing output, only returns the directory instance.

**bean-cat**

Convert the denoted object to a string. By defult the result is printed on the output and returned as the result, but
the printing can be suppressed with the `quiet=true` option.

* quiet = true | false*. Prevents writing output, only returns the string.

**bean-rslv**

Convert a pathname to the object itself. We can obtain a direct reference to the object in this way.
This command is the bridge to other command libraries because you can pass values to other command libraries
in this way.

**bean-call**

Call java methods on an object denoted by a path. The target object should be denoted by a pathname. The method can be 
specified by a method instance or by a name (and a lookup will occur). The arguments are not resolved, they are passed 
directly to the method. You can use the 'rslv' command above to accomplish argument resolution as well. 
It has the form (bean-call $bean method arg1 ... argN).

**bean-upd**

Set the property of a JavaBean. The argument should be a pair, a propname=value combination.
