# Scripty Command Language
## Embed a Scripting Language in your Application

Contents

1. Description
2. Example
3. Build
4. Use Cases
5. Scripty Syntax
6. Available Scripty Libraries
7. Create a Scripty Command or Macro
8. Using the Command Line
9. Using the Debug Commands

### 1. Description

Scripty is an interpreter that can be used to provide a simple script language that can be embedded inside an application. Situations where Scripty might be useful:

* Command line parsing during startup of the embedding application.
* Application configuration scripts.
* Provide a command line GUI to the embedding application.
* Add automation facilities to the embedding application.

Furthermore, you have full control over the power of Scripty. By default only the language syntax and control structures are present. You can augment the power of Scripty by selecting the command libraries explicitly.

### 2. Example: Create a read-eval-print loop

This is only one of the possible ways to embed Scripty in an application. We will use Scripty as the top level REPL.

This wil create a read-eval-print loop that automatically detects if the application is running on a graphical system or not. If the application is started on a text-only system, a text read-eval-print loop is started, if the application is started in a windowing system a graphical command editing application is started.

```Java
public static void main(String[] aArgs)
throws ExtensionException
{
    ScriptyAutoRepl lRepl = new ScriptyAutoRepl();
    lRepl.addLibraryClasses(PrintLibrary.class, MathLibrary.class);
    lRepl.startLoop();
}
````

![Screenshot](https://www.dropbox.com/s/sg8i0lwof6xgv94/scripty-autorepl-screenshot.jpg)

Note that you have to provide a list of command libraries to Scripty, you have complete control over the commands that will be available.

In order to create your own commands that access the business logic you have to create a class and annotate its methods. The class and the methods can be static or not.

```Java
@ScriptyLibrary(name="MyLib", type=ScriptyLibraryType.STATIC)
public class PrintLibrary
{
    @ScriptyCommand(name="hi")
    public void method123()
    {
        System.out.println("Hi there!");
    }
}
```

### 3. Build

The project is based on maven.

```
mvn clean install
```

### 4. Use cases

#### 4.1. Text REPL

A classic text oriented REPL that can be run from the command line. It can be run on systems where there is no windowing system installed (in the console).

Example:

```Java
// Text REPL
// A static function as a command
//
public class HelloLib 
{
   @ScriptyCommand
   public static void hello(IContext aCtx)
   {
      Writer lWriter = (Writer) aCtx.get("*output");
      lWriter.writeln("Hello World! (from a static command)"); 
   }

   public static void main (String[] aArgs) 
   {
      ScriptyTextRepl lRepl = new ScriptyTextRepl();
      lRepl.addLibrary(HelloLib.class);
      lRepl.start(); 
   }
}
```

#### 4.2. GUI REPL

A Swing application with a main window frame, an area to enter commands and a text area to see the results.

TODO Create Documentation

#### 4.3. Automatic REPL

GUI or Text, depending on the runtime capabilities or the choice of the user.

TODO Create Documentation

#### 4.4. Embedded GUI Panel

The developer creates a Swing component and embeds this component in a larger interface.

TODO Create Documentation

#### 4.5. Stream Processor (file scripts)

* Allow the developer to create an evaluator that continuously parses expressions from a stream and evaluates the expressions if they are complete.
* This is for evaluating large streams of expressions, only the first expression will be parsed and as soon it is complete it will be executed.
* You can feed it a String as well.
* Typically, the processor gobbles up and assembles lines into expressions at its own pace. 
* It is more like off-line processing of scripts.

TODO Create Documentation

#### 4.6. Instrumented Application (REPL server)

The developer embeds a command line server in a standalone application so that the running application can be contacted using a standard telnet session. Instrument an application with a mini server so you can monitor what it is up to.

TODO Create Documentation 

#### 4.7. Embedded Interpreter 

Create a Scripty interpreter inside the application and invoke the expression evaluator manually. This technique can be used to evaluate command line arguments.

TODO Create Documentation

### 5. Scripty Syntax
#### 5.1. Expression Evaluation

Each Scripty expression consists of a command and zero or more arguments. All arguments are evaluated first, from left to right, and then the command is applied to the evaluated arguments.

The command must evaluate to the name of a built-in form, the name of a user defined function, the name of a command or a lambda expression.

#### 5.2. Forms

**and**

Shortcut boolean evaluation operator.

Example

```
> defvar x 5
> print (and (> $x 1) (< $x 10))
true
```

**bound?**

A test to see whether a binding exists. You need to provide the name of the binding.

Example

```
> defvar x 5
> print (bound? x)
true 
> print (bound? y)
false
```

**defun**

User defined global functions.(defun name (<params>) <expr>) User defined functions, they are bound in the same context as the variables are. Functions are bound in the global context with this form.
name should be a string. The name is not evaluated.
(<params>)The parameter list a list of strings. The list is not evaluated.

Example

A recursive definition of the factorial function. It is not the most efficient implementation it is to demonstrate the language.

```
(defun fact (n)
    (if (> $n 0)
        (* $n (fact (- $n 1)))
        1))
```

**defvar**

This form creates a global binding. It evaluates the value before setting, the result is the value: (defvar name val) | (defvar name=val). The value can be changed with set.
Example 1

```
> defvar pi 3.141592653
> print $pi
3.141592653
```

Example 2

```
> defvar pi=3.141592653
> print $pi
3.141592653
```

**eq**

The comparison operator has the same semantics as the 'equals' method in Java. It is the only data inspection that is implemented in the Eval. It is included because it is a standard Java Object method, applicable to all instances.
eval

Evaluate an expression. This is less important for a command language, but it is here for completeness sake. Eval works on lists, it does not work on string representations of expressions.
Example

In this example we make use of the optional library containing list manipulation commands, the 'list' command creates a list. If we want to execute this command we created on the fly, we can use the 'eval' command.

```
> eval (list print Hello)
Hello
```

**funcall**

This is the official way to call a user defined function (funcall name <args>). But luckily there is a shorthand call of the form (name arg-list). This form will lead to a function call if there was no registered command with the same name. The first 'funcall' form will only work for user defined functions and lambdas, not for commands. The second form will work for commands as well. Commands are not considered to be real functions that is why there is a difference.
name should be a string and is not evaluated.
<args> The arguments in a separate list.

Example

```
> defun myfunc () (print Hello)
> funcall myfunc
Hello
```
 
**get**

This form retrieves a binding. It does not do any evaluation: (get name) or the shorthand notational convenience $name does exactly the same thing. It does not do Perl-like interpolation in strings, don't let the notation mislead you.

Example The two forms are equivalent.

```
> defvar place World
> print Hello $place
Hello World 
> print Hello (get place)
Hello World
```

**if**

The expression has the form (if <bool-expr> <then-expr> <else-expr>) . It is a special form because the evaluation of the then-expr or else-expr depends on the outcome of the test. Since the evaluation order of all boolean constructs is deviant from normal evaluation they have to be built into the core.

Example

```
> if true (print ola) (print alo)
ola
```

**lambda**

A nameless function, it contains a reference to the lexical context where it was defined. So this creates closures. Again, this is less important for Scripty as a command language, but it is here for completeness sake. You cannot pretend that Scripty is not complete.

Example

```
> defvar myfunc (lambda (text) (print $text))
> funcall $myfunc Wonderful!
Wonderful!
> $myfunc Incredible!
Incredible!
```
 
**let, let***

The forms let, let* define variables locally: (let ((var val) | var=val | var ...) expr). The first form without asterisk is non recursive, the right sides are evaluated first before assignment, whereas in the second form the assignments are performed sequentially, a latter assignment can make use of a former one.
Both forms create a temporary context with new bindings in which the expression is evaluated. The new bindings will be available for the expression. When the evaluation is finished, the context and the bindings in it will be removed.

A lambda within a let context will keep the access to the defining context.

Example

```
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
```

**not**

Shortcut boolean evaluation operator.

**or**

Shortcut boolean evaluation operator.

**progn**

The progn form accepts a list of expressions which are evaluated in order: (progn expr1 expr2 ...). It is a sequence of expressions.
Example

```
> progn (print Hello) (print World)
Hello 
World
```
 
**quote**

The quote form prevents evaluation of an expression: (quote <expr>) or the shorthand '<expr>. It is necessary to provide this construct so that the user can use unevaluated expressions to describe data structures or other parameters that can be provided to the commands.
Example The result of the evaluation is a list, the print command is not executed.

```
> print (quote (print Hello))
[print, Hello]
```

**set**

set changes an existing binding. It evaluates the value before setting, the result is the value: (set name val) | (set name=val). Set generates an error if the binding does not exist. A binding can initially be created using one of the following constructs. After a binding is created it can be modified with set.
A defvar which creates/overwrites a global binding.
A defun which (re-)binds a global variable to a lambda.
A let or let* block which adds a number of bindings for the duration of a block.
Some commands add something to the context too. It is up to the various commands to specify this.

Example 1: A binding must exist before a set can be executed.

```
> set newvar 13
ERROR: There is no binding for 'newvar' in the context.
-> [set, newvar, 13]
```

Example 2 An existing binding is changed here.

```
> defvar myvar 17
> set myvar=3
> print $myvar
3 
```

Example 3: The form works on the binding that can be 'seen' within the scope of the assignment.

```
> defvar myvar 17
> let (myvar=3) (progn (set myvar=7) (print $myvar))
7 
> print $myvar
17
```

**while**

A classic while loop (while <bool-expr> <expr>).
Example

```
> while (> $x 0) (progn (set x (- $x 1)) (print $x))
2 
1 
0
```

#### 5.3. Data Types

Scripty itself only uses strings and lists. No other data types are built into the scripting language (no booleans, no numeric values, ...). How can such a language be of any use? The commands can return any type of reference as a result. This means that whatever Java instance can be stored in the Scripty context, and although Scripty itself cannot utilize this value, it can pass these variables to other commands.

The file commands in the file library print out file information on the standard output, but they also return java.io.File objects as a result of the call. These instances can be passed to the other commands or to your own commands. You can utilize the file commands to scan a directory and pass the File instances to your own extensions.

1. Strings. They can be quoted "this is a string" and in this form it can contain the usual control characters. In unquoted form, Scripty will create a string for each delimited series of characters.
2. Pairs. They have the form left=right, and are instances of the class Pair. It is up to the commands process these.
3. Lists are formed automatically.

#### 5.4. Boolean Expressions

Scripty takes the same approach as JavaScript for boolean evaluation. There are 'truthy' values and 'falsy' values.

* If the object is a **boolean**, then it will evaluate to its own value.
* **Collections** evaluate to true if they are non empty, and to false if they are empty.
* **Numerical** values byte, short, integer and long will evaluate to true if different from zero and false otherwise.
* **String** values are interpreted. The strings (case insensitive comparison) "true", "yes", "t", "y", "on" are interpreted as true, other values as false.
* Any other **Object** is interpreted as true, a null value is interpreted as false.

Or stated otherwise:

* **"truthy"**: yes, y, on, true, t, non-null
* **"falsy"**: 0, no, off, false, null, empty collection, non-empty strings different from the negative values listed before.

#### 5.5. Remarks

Scripty is not Lisp. The syntax is the same, most of the concepts and ideas were borrowed from Lisp. Scripty was not built to be a Lisp clone, it was built to be extended with your own Java commands. Some of the differences that might trip you up are listed below.

* Lists are Java lists and not conses. So list semantics is different (and maybe less efficient). There is no 'nil' concept; a list does not end with a nil, a nil is not the same as an empty list.
* No separate name spaces for different constructs, there is only a single context stack.
* Contexts have side effects, bindings can be changed.
* Only strings are provided, there is no 'symbol' concept. If an application wants e.g. numbers for calculation, the commands should parse the strings.
* Binding context lookup order. Scoping is lexical. The global context is always available for everybody, there is no need to introduce dynamic scoping.
  1. Call context, arguments are bound to parameters. This context is especially created for this call. It contains all local bindings.
  2. Lexical (static) context, where the function or lambda was defined. It is the closure of the lambda.

### 6. Available Libraries

The scripty component contains a number of pre-defined command libraries that are at your disposal.

#### 6.1. Bean Library

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

#### 6.2. Debugger Library

Debug commands to interactively debug Scripty programs.

**dbg-expr**

Start the debugger with the expression. The debugger is halted in the beginning of evaluation. Starting a new debugging session will end the previous session if one was active.

**dbg-eval**

Evaluate an expression in the context of the top level stack frame. This allows you to examine or modify the frame context during debugging. Use dbg-ctx to print this context and to examine the values. While you are debugging you can execute stuff in the current context where the debugger is halted.

**dbg-step**

Step into an expression, it takes the smallest step possible.

**dbg-stepin**

A synonym for dbg-step, it was added to be the inverse for dbg-stepout.
dbg-back

Take a step back, it might not work as expected because it does not undo bindings, nor other side effects. It could be useful to replay some sequences.
dbg-stepout

Run until the current expression on top of the stack is completely evaluated and then execute the return statement. We return to the previous stack level. It always evaluates the current frame and then goes to the previous frame. This operation always reduces the stack.

**dbg-stepover**

Evaluate the next parameter without stepping trough it or execute the expression. We remain positioned at the same stack level. Use this if you want to evaluate each parameter in turn, without seeing the details of it.

**dbg-stack**

Print the current stack. Optional arguments:

* quiet = true | false*. Prevents writing output, only returns the instance.

**dbg-terminate**

Terminate the debugging session.

**dbg-raise**

Raise a CommandException, it is useful to debug certain constructs, or to examine the result of an exception in a certain expression.

* arg: an optional message.

**dbg-run**

Keep on running.

**dbg-runresult**

Keep on running until a result has been produced.

**dbg-runready**

Keep on running until all parameters in the current frame have been evaluated, and the main expression is ready for being executed. Use this if you are not interested in detailed evaluation of the parameters.

**dbg-restart**

Reset the debugger to the beginning of the evaluation. You can restart the debugging of an expression with this. Side effects will not be undone though.

**dbg-dropframe**

Drop the top level stack frame. It can be useful to redo the evaluation of a sub expression.

**dbg-moresteps?**

Check if more steps could be executed in the current debugging session.

**dbg-result?**

Check if the current debugging session has reached a result.

**dbg-exception?**

Check if the current debugging session was halted with an exception. If so, the exception will be remembered, you can get it with dbg-exception.

**dbg-result**

Get the result of the current debugging session if a result has been reached. Otherwise null will be returned, note that the result might be null, in order to maker sure if this is the result or it stands for an empty result use the dbg-result? command.

**dbg-exception**

Get the exception that was thrown during the current debugging session if the expression under scrutiny effectively raised an exception. If no exception was raised, null will be returned.

**dbg-ctx**

Print the context of the top of the stack. You can examine all the bindings at that point during the evaluation of the expression. Use dbg-eval to manipulate this context. Optional arguments:

* quiet = true | false*. Prevents writing output, only returns the instance.

**bpt-func**

Create a break point that breaks when a function with a specified name appears on top of the stack. Required arguments:

* The function name.

Optional arguments:

* name: choose a name for this break point, otherwise a name will be generated of the form bp[x] where [x] is an integer sequence.

**bpt-when**

Create a break point that breaks when a condition is met. The condition is evaluated in the context of the current frame. Required arguments:

* A conditional expression.

Optional arguments:

* name: A user name for this break point, otherwise it will be a generated one.

**bpt-stack**

Create a break point that breaks when the stack exceeds the specified depth. Required arguments:

* The stack size threshold.

Optional arguments:

* name: A user name for this break point, otherwise it will be a generated one.

**bpt-and, bpt-or**

Combine lists of break points into a new break point.

**bpt-not**

Create a new break point by inverting an existing one.

**dbg-addbreakpoint**

Add a breakpoint. Required argument: a break point, created with bpt-func, bpt-stack, ...

Example: (dbg-addbreakpoint (bpt-func fac))

**dbg-breakpoints**

List the existing break points known by the debugger.

**dbg-removebreakpoint**

Remove a break point. Required argument: the name of the break point.

Example (dbg-removebreakpoint bp0)

**dbg-enablebreakpoint**

Enable or disable a named break point. Required arguments:

* The break point name.
* true | false.

Example: (dbg-enablebreakpoint bp0 true)

dbg-clearbreakpoints

Remove all break points from the debugger.

**Macro's**

These macros are provided for your convenience. You have to load these explicitly before you can use them by issuing the command (load cp:/dbgutil.lsp).

* **e 'expr**, Start debugging an expression. Don't forget the quote!
* **t**, Terminate the debug session.
* **x**, Stack dump.
* **s**, Step + stack.
* **b**, Backstep + stack.
* **sover**, Step over (the parameter) + stack.
* sout, Step out (of the expression) + stack.
* **r**, Run until finished.
* **rready**, Run until the parmeters of the topmost expression are evaluated and the expression itself can be executed.
* **rresult**, Run until a result is reached.
* **result**, Print the result.
* **ctx**, Print the current context (of the topmost expression).
* **v 'expr**, View an expression, evaluated in the topmost context. Don't forget the quote!
* **restart**, Restart evaluation, start from the beginning.
* **df**, Drop the topmostframe + stack.

#### 6.3. Editor Library

Open a GUI text pane to edit some text.

#### 6.4. Exit Library

Command to exit the application.

#### 6.5. FileDialog Library

Commands to open a dialog to open or save a file.

#### 6.6. File Library

UNIX commands to navigate the file system.

* It is intentionally based on the Java File object (for re-usability in other commands). As a consequence, a move only accepts 2 files, no wildcards.
* No wild card globbing by the shell. The ls provides a grep on the short name. The find command provides a lookup facility.
* Delete was not implemented for safety reasons.

**cd**

Change the current directory.

**pwd**

Print the current directory and return the File object.

**ls**

Print a listing of the current directory and return it as an array.

* grep=regexp: applied to the absolute pathname.
* quiet=true|*false.
* files=*true|false : include files.
* dirs=*true|false : include directories.
* exec=lambda: process the files using a lambda. Processing is done after grepping and filtering.

**cat**

Not implemented.

**rslv**

Resolve is the same as a 'quiet' pwd, but pwd only returns directories, whereas rslv can also resolve to files. This command is *very* important for integration with other libraries. The other command libraries should not be dependent on this one, they can simply request a File argument and this command can resolve the path to the File.

**mv**

Rename a file. It does not use wild cards since the implementation is based on the Java File class which does not accept wild cards.

#### 6.7. List Library

Some Lisp like commands that act on lists. This module provides the basic Data manipulation commands, they are not built-in but provided as a separate command library. The semantics deviates from common lisp because we based our lists on Java lists and not on cons constructs. Consing for example modifies the existing list, while in Lisp it creates a new version, the original binding refers to the sublist. This can lead to surprising results if one is used to the original Lisp semantics.

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

#### 6.8. Load Library

Load and reload external scripts into the environment.

**load**

Load and execute one or more scripts. If the file exists and is readable, it will be remembered for the reload.

```
(load file | classpath:/resource | cp:/resource ...)
```

**reload**

Reload previously loaded files. A file is remembered if it existed and was readable.

```
(reload)
```

#### 6.9. Map Library

Manipulate map data structures.

**map-create**

Create an empty map. You can immediately insert pairs.

(map-create key=val | str | m ...)

**map?**

Test whether an object is a map.

```
(map? m)
```

**map-set**

Insert the key/value in a map.

```
(map-set m key val)
```

**map-get**

Get the value bound by the key.

```
(map-get m key)
```

**map-key?**

Test whether a key is present.

```
(map-key? m key)
```

**map-keys**

Get the list of keys.

```
(map-keys m)
```

**map-values**

Get the list of values.

```
(map-values m)
```

**map-clear**

Make the map empty.

```
(map-clear m)
```

**map-size**

Get the number of entries in the map.

```
(map-size m)
```

#### 6.10. Math Library

The JavaScript approach was taken to implement the math command library, there is only one type that is the BigDecimal type. Some unconventional notation was chosen, like the <~ and >~ operators for comparison, this is because using the ''=" here would conflict with Scripty pairs which have the form name=value.

If you provide strings, they will be converted to numbers first, it is ok to provide string arguments.

**abs**

Absolute value.

**+, -, *, /, ^**

The conventional mathematics operators.

**fin**

Convert the number to a String with 2 decimals as it is used in financial information (invoices and the lot).

**float->int**

Convert to an integer representation.

**zero?**

Test if a number is zero.

**~, >, >~, <, <~**

Numerical comparison. The '~' stands for '=' but the last one cannot be used because it conflicts with Scripty pairs, the Scripty parser will try to create a pair when it encounters a '='.

**number?**

Check if the thing you passed as an argument can be converted to a float.

**rem**

Remainder.

#### 6.11. Pair Library

A pair is a Scripty construct 'key=value'. These commands let you manipulate this data structure. A Pair is an immutable parser artifact. There are only test functions and getter functions.

**pair?**

Test whether an object is of type pair.

```
(pair? obj)
```

**pair-left**

Get the left part of a pair.

```
> print (pair-left key=value)
key
```

**pair-right**

Get the right part of a pair.

```
> print (pair-right key=value)
value
```

#### 6.12. Print Library

print commands.

#### 6.13. Record Editor Library

A record editor that allows key-value pair editing in a GUI property editor.

#### 6.14. String Library

String manipulation commands.

**str?**

Check if an arbitrary object is a string.

**str-trim**

Trim the whitespace on both ends of the string.

**str-format**

It has the same behaviour as the Java version.

```
> print (str-format "%s = %s" "1 + 1" (+ 1 1))
1 + 1 = 2
```

**str-match**

Do a single match, the result is a list of matched groups. Even is there is no group in the pattern, the global match is always available.

```
> print (str-match "rosa\S*\b" "rosa rosam rosas")
[rosa]
```

**str-match***

Repeatedly match the pattern. The result is a list of lists of matches. It is the same as str-match but it is applied repeatedly to the string.

```
> print (str-match* "rosa\S*\b" "rosa rosam rosas")
[[rosa], [rosam], [rosas]]
```

**str-match?**

Check if a string complies to a pattern. The result is a boolean.


### 7. Create a Scripty Command or Macro
#### 7.1. Defining commands

**Static methods** in a class. Scripty will scan the static methods of the class for annotations and transform each annotated method in a Command or macro.

* Most efficient commands, no extra memory needed the commands are completely stateless. Commands can only share information trough the context.

**Plain objects** with one or more annotated methods. Scripty will scan the instance for both instance methods with annotations and also static methods. Scripty will call the static method or the method on the instance when the command is executed.

* Instances with command methods can have shared state between the command methods. The instance command methods have access to a private memory which is only accessible by the command methods within the same instance. They also have access to the context just like the static method commands.

**Classic command instances**, implementations of the ICommand interface. This type of command can have argument list annotations to do automatic argument checking, but they cannot have parameter mapping specifications since the interface method will be called, and there is no room for negotiation. Classic commands and macros will excel in speed, since there will not be overhead of the dynamic argument handling each time it is called.

Commands can be grouped in libraries. If a command/macro is not specifically associated with a library it will be put in the "global library".

If the command or macro name is not provided, the method name will be used automatically as the name of the command or macro.

```Java
@ScriptyLibrary(name="library-name", onlyStatic=true|false)

@ScriptyCommand(name="command-name")
@ScriptyMacro(name="macro-name")
```

#### 7.2. Command arguments

Without parameter mappings the command invoker will look at the signature of the command method and provide following resources automatically. All other parameters will receive a null value.

* Object[] will receive the complete parameter list, including the command name in position 0.
* Eval will receive the expression evaluator.
* IContext will receive the current context.
* (CommandRep/MacroRepo)

#### 7.3. Macro arguments

The first list parameter will receive the expression. All other values will be set to null.

### 8. Using the Command Line
#### 8.1. Optional Outer Parentheses

Outer parenthesis can be omitted on the command line as a convenience for short commands. Using the parenthesis is off course allowed, it is the only way to enter nested expressions.

In script files the outer parentheses are obligatory.

Example: These two expressions have the same meaning on the command line.

```
> (print Hello World)
Hello World 
> print Hello World
Hello World
```

#### 8.2. Multi Line Commands

Commands can be spread over multiple lines. This can happen automatically when the command is not complete. You can also ask for this explicitly by terminating the line with a backslash. When you want to quit the multi line command without execution you can issue the break command. It is not really a command since it is directly interpreted by the read-eval-print loop. So you cannot use this command in external scripts.

Example 1: Automatic multi line command.

```
> (print Hello
+ > World
+ > )
Hello World
```

Example 2: Explicit multi line command with backslashes at the end of the line.

```
> print Hello \
+ > World \
+ > )
Hello World
```

Example 3: Terminating a multi line command without execution.

```
> print \
+ > Hello \
+ > break
Canceling the command.
```

#### 8.3.print

Print stuff on the standard output. Scripty does not automatically print the result of an evaluation on the command line, often you have to use the print command explicitly. It is not uncommon for a command to show something on the standard output and to have something else as the result of the evaluation.

#### 8.4. exit

Obviously, this command terminates the application.

### 9. Using the Debug Commands
#### 9.1. Starting a debug session

If we want to debug the expression "2 . 3 + 7" we have to execute following commands:

```
> dbg-expr (+ (* 2 3) 7)
> dbg-step
> dbg-stack
```

The result is a stack of expressions, the topmost expression is the most recent one, this expression has to be evaluated before the lower ones. In our example, the + expression will be evaluated before the complete expression.

```
+
[+, [*, 2, 3], 7]:0 ~ [null, null, null]
```

#### 9.2. Interpreting the stack trace

If we keep on executing (progn (dbg-step) (dbg-stack))for a while, we will get stack dumps of the form:

```
2 ==> 2
[*, 2, 3]:1 ~ [*, null, null]
[+, [*, 2, 3], 7]:1 ~ [+, null, null]
```

This is how we should understand this:

* We are now evaluating string '2', and the result is '2' since numbers evaluate to themselves.
* We were evaluating '2' because in the previous expression on the stack we are evaluating (* 2 3) and we already evaluated the *.

The list right of the '~' shows the evaluated parts of the expression.