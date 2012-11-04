# Scripty Command Language
## Embed a scripting language in your application

Contents

1. [Description](toc_1)
2. [Example](toc_2) 
3. [Build](toc_3)
4. [Use Cases](toc_4)
5. [Scripty Syntax](toc_5)
6. [Available Scripty Libraries](toc_6)
7. [Create a Scripty Command or Macro](toc_7)

### <a id="toc_1"></a> 1. Description

Scripty is an interpreter that can be used to provide a simple script language that can be embedded inside an application. Situations where Scripty might be useful:

* Command line parsing during startup of the embedding application.
* Application configuration scripts.
* Provide a command line GUI to the embedding application.
* Add automation facilities to the embedding application.

Furthermore, you have full control over the power of Scripty. By default only the language syntax and control structures are present. You can augment the power of Scripty by selecting the command libraries explicitly.

### <a id="toc_2"></a> 2. Example: Create a read-eval-print loop

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

### <a id="toc_3"></a> 3. Build

The project is based on maven.

```
mvn clean install
```

### <a id="toc_4"></a> 4. Use cases

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

### <a id="toc_5"></a> 5. Scripty Syntax
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

##### 5.5. Remarks

Scripty is not Lisp. The syntax is the same, most of the concepts and ideas were borrowed from Lisp. Scripty was not built to be a Lisp clone, it was built to be extended with your own Java commands. Some of the differences that might trip you up are listed below.

* Lists are Java lists and not conses. So list semantics is different (and maybe less efficient). There is no 'nil' concept; a list does not end with a nil, a nil is not the same as an empty list.
* No separate name spaces for different constructs, there is only a single context stack.
* Contexts have side effects, bindings can be changed.
* Only strings are provided, there is no 'symbol' concept. If an application wants e.g. numbers for calculation, the commands should parse the strings.
* Binding context lookup order. Scoping is lexical. The global context is always available for everybody, there is no need to introduce dynamic scoping.
  1. Call context, arguments are bound to parameters. This context is especially created for this call. It contains all local bindings.
  2. Lexical (static) context, where the function or lambda was defined. It is the closure of the lambda.

### <a id="toc_6"></a> 6. Available Libraries

The scripty component contains a number of pre-defined command libraries that are at your disposal.

#### 6.1. Bean Library

Access JavaBeans as a nested directory structure.

#### 6.2. Debugger Library

Debug commands to interactively debug Scripty programs.

#### 6.3. Editor Library

Open a GUI text pane to edit some text.

#### 6.4. Exit Library

Command to exit the application.

#### 6.5. FileDialog Library

Commands to open a dialog to open or save a file.

#### 6.6. File Library

UNIX commands to navigate the file system.

#### 6.7. List Library

Lisp commands to manipulate list structures.

#### 6.8. Load Library

Load and reload external scripts into the environment.

#### 6.9. Map Library

Manipulate map data structures.

#### 6.10. Math Library

Math commands.

#### 6.11. Pair Library

A pair is a Scripty construct 'key=value'. These commands let you manipulate this data structure.

#### 6.12. Print Library

print commands.

#### 6.13. Record Editor Library

A record editor that allows key-value pair editing in a GUI property editor.

#### 6.14. String Library

String manipulation commands.

### <a id="toc_7"></a> 7. Create a Scripty Command or Macro
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

