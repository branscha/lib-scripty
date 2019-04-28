# Scripty Syntax

This is a description of the core Scripty language without any of the command libraries, this describes the base functionality. You have to augment the core with one or more command libraries.

## Expression Evaluation

Each Scripty expression consists of a command and zero or more arguments. All arguments are evaluated first, from left to right, and then the command is applied to the evaluated arguments. The command must evaluate to the name of a built-in form, the name of a user defined function, the name of a command or a lambda expression.

## Data Types

Scripty itself only uses strings and lists. No other data types are built into the scripting language (no booleans, no numeric values, ...). How can such a language be of any use? The commands can return any type of reference as a result. This means that whatever Java instance can be stored in the Scripty context, and although Scripty itself cannot utilize this value, it can pass these variables to other commands.

The file commands in the file library print out file information on the standard output, but they also return java.io.File objects as a result of the call. These instances can be passed to the other commands or to your own commands. You can utilize the file commands to scan a directory and pass the File instances to your own extensions.

1. **Strings**. They can be quoted "this is a string" and in this form it can contain the usual control characters. In unquoted form, Scripty will create a string for each delimited series of characters.
2. **Pairs**. They have the form `key=value`, and are instances of the class Pair. It is up to the commands process these. They are frequently used for named arguments.
3. **Lists** are formed automatically.

## Boolean Expressions

Scripty takes the same approach as JavaScript for boolean evaluation. There are 'truthy' values and 'falsy' values.

* If the object is a **boolean**, then it will evaluate to its own value.
* **Collections** evaluate to true if they are non empty, and to false if they are empty.
* **Numerical** values byte, short, integer and long will evaluate to true if different from zero and false otherwise.
* **String** values are interpreted. The strings (case insensitive comparison) "true", "yes", "t", "y", "on" are interpreted as true, other values as false.
* Any other **Object** is interpreted as true, a null value is interpreted as false.

Or stated otherwise:

* **"truthy"**: yes, y, on, true, t, non-null
* **"falsy"**: 0, no, off, false, null, empty collection, non-empty strings different from the negative values listed before.

## Final Remarks

Scripty is not Lisp. The syntax is the same, most of the concepts and ideas were borrowed from Lisp. Scripty was not built to be a Lisp clone, it was built to be extended with your own Java commands. Some of the differences that might trip you up are listed below.

* Lists are Java lists and not conses. So list semantics is different (and maybe less efficient). There is no 'nil' concept; a list does not end with a nil, a nil is not the same as an empty list.
* No separate name spaces for different constructs, there is only a single context stack.
* Contexts have side effects, bindings can be changed.
* Only strings are provided, there is no 'symbol' concept. If an application wants e.g. numbers for calculation, the commands should parse the strings.
* Binding context lookup order. Scoping is lexical. The global context is always available for everybody, there is no need to introduce dynamic scoping.

  1. Call context, arguments are bound to parameters. This context is especially created for this call. It contains all local bindings.
  2. Lexical (static) context, where the function or lambda was defined. It is the closure of the lambda.

## Form Reference

**and**

Shortcut boolean evaluation operator.

Example

	> defvar x 5
	> print (and (> $x 1) (< $x 10))
	true

**bound?**

A test to see whether a binding exists. You need to provide the name of the binding.

Example

	> defvar x 5
	> print (bound? x)
	true 
	> print (bound? y)
	false

**defun**

User defined global functions. `(defun name (<params>) <expr>)`. These are bound in the same context as the variables are. Functions are bound in the global context with this form.

* name should be a string. The name is not evaluated.
* `(<params>)` The parameter list a list of strings. The list is not evaluated.

Example

A recursive definition of the factorial function. It is not the most efficient implementation it is to demonstrate the language.


	(defun fact (n)
	    (if (> $n 0)
	        (* $n (fact (- $n 1)))
	        1))


**defvar**

This form creates a global binding. It evaluates the value before setting, the result is the value: (defvar name val) | (defvar name=val). The value can be changed with set.
Example 1

	> defvar pi 3.141592653
	> print $pi
	3.141592653

Example 2

	> defvar pi=3.141592653
	> print $pi
	3.141592653

**eq**

The comparison operator has the same semantics as the 'equals' method in Java. It is the only data inspection that is implemented in the Eval. It is included because it is a standard Java Object method, applicable to all instances.

**eval**

Evaluate an expression. This is less important for a command language, but it is here for completeness sake. Eval works on lists, it does not work on string representations of expressions.

Example

In this example we make use of the optional library containing list manipulation commands, the 'list' command creates a list. If we want to execute this command we created on the fly, we can use the 'eval' command.

	> eval (list print Hello)
	Hello

**funcall**

This is the official way to call a user defined function `(funcall name <args>)`. But luckily there is a shorthand call of the form `(name arg-list)`. This form will lead to a function call if there was no registered command with the same name. The first 'funcall' form will only work for user defined functions and lambdas, not for commands. The second form will work for commands as well. Commands are not considered to be real functions that is why there is a difference.

 * name should be a string and is not evaluated.
 * `<args>` The arguments in a separate list.

Example

	> defun myfunc () (print Hello)
	> funcall myfunc
	Hello
 
**get**

This form retrieves a binding. It does not do any evaluation: (get name) or the shorthand notational convenience $name does exactly the same thing. It does not do Perl-like interpolation in strings, don't let the notation mislead you.

Example The two forms are equivalent.

	> defvar place World
	> print Hello $place
	Hello World 
	> print Hello (get place)
	Hello World

**if**

The expression has the form `(if <bool-expr> <then-expr> <else-expr>)` . It is a special form because the evaluation of the then-expr or else-expr depends on the outcome of the test. Since the evaluation order of all boolean constructs is deviant from normal evaluation they have to be built into the core.

Example

	> if true (print ola) (print alo)
	ola

**lambda**

A nameless function, it contains a reference to the lexical context where it was defined. So this creates closures. Again, this is less important for Scripty as a command language, but it is here for completeness sake. You cannot pretend that Scripty is not complete.

Example

	> defvar myfunc (lambda (text) (print $text))
	> funcall $myfunc Wonderful!
	Wonderful!
	> $myfunc Incredible!
	Incredible!
 
**let, let***

The forms let, let* define variables locally: `(let ((var val) | var=val | var ...) expr)`. The first form without asterisk is non recursive, the right sides are evaluated first before assignment, whereas in the second form the assignments are performed sequentially, a latter assignment can make use of a former one.

Both forms create a temporary context with new bindings in which the expression is evaluated. The new bindings will be available for the expression. When the evaluation is finished, the context and the bindings in it will be removed.

A lambda within a let context will keep the access to the defining context.

Example

	> defvar a=uno
	> defvar b=duo
	> let (a=xxx b=$a) (print a: $a b: $b)
	a: xxx b: uno ;; Variable b gets the value of the outer a.
	> print a: $a b: $b
	a: uno b: duo ;; The bindings were local to the let.
	> let* (a=xxx b=$a) (print a: $a b: $b)
	a: xxx b: xxx ;; Variable b gets the value of the inner a. 
	> print a: $a b: $b
	a: uno b: duo ;; The bindings were local to the let*.

**not**

Shortcut boolean evaluation operator.

**or**

Shortcut boolean evaluation operator.

**progn**

The progn form accepts a list of expressions which are evaluated in order: `(progn expr1 expr2 ...)`. It is a sequence of expressions.

Example

	> progn (print Hello) (print World)
	Hello 
	World
 
**quote**

The quote form prevents evaluation of an expression: `(quote <expr>)` or the shorthand `'<expr>`. It is necessary to provide this construct so that the user can use unevaluated expressions to describe data structures or other parameters that can be provided to the commands.

Example: The result of the evaluation is a list, the print command is not executed.

	> print (quote (print Hello))
	[print, Hello]
	
**set**

set changes an existing binding. It evaluates the value before setting, the result is the value: `(set name val) | (set name=val)`. Set generates an error if the binding does not exist. A binding can initially be created using one of the following constructs. After a binding is created it can be modified with set.

* A defvar which creates/overwrites a global binding.
* A defun which (re-)binds a global variable to a lambda.
* A let or let* block which adds a number of bindings for the duration of a block.

Some commands add something to the context too. It is up to the various commands to specify this.

Example 1: A binding must exist before a set can be executed.

	> set newvar 13
	ERROR: There is no binding for 'newvar' in the context.
	-> [set, newvar, 13]

Example 2: An existing binding is changed here.

	> defvar myvar 17
	> set myvar=3
	> print $myvar
	3 

Example 3: The form works on the binding that can be 'seen' within the scope of the assignment.

	> defvar myvar 17
	> let (myvar=3) (progn (set myvar=7) (print $myvar))
	7 
	> print $myvar
	17

**while**

A classic while loop `(while <bool-expr> <expr>)`.

Example

	> while (> $x 0) (progn (set x (- $x 1)) (print $x))
	2 
	1 
	0



