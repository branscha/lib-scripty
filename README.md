# Scripty embeddable command language
## Embed a scripting language in your application
### Description

Scripty is an interpreter that can be used to provide a simple script language that can be embedded inside an application. Situations where Scripty might be useful:

* Command line parsing during startup of the embedding application.
* Application configuration scripts.
* Provide a command line GUI to the embedding application.
* Add automation facilities to the embedding application.

Furthermore, you have full control over the power of Scripty. By default only the language syntax and control structures are present. You can augment the power of Scripty by selecting the command libraries explicitly.

### Example: Create a read-eval-print loop

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

### Build

The project is based on maven.

```
mvn clean install
```

### Use cases

#### Text REPL

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

#### GUI REPL

A Swing application with a main window frame, an area to enter commands and a text area to see the results.

TODO Create Documentation

#### Automatic REPL

GUI or Text, depending on the runtime capabilities or the choice of the user.

TODO Create Documentation

#### Embedded GUI Panel

The developer creates a Swing component and embeds this component in a larger interface.

TODO Create Documentation

#### Stream Processor (file scripts)

* Allow the developer to create an evaluator that continuously parses expressions from a stream and evaluates the expressions if they are complete.
* This is for evaluating large streams of expressions, only the first expression will be parsed and as soon it is complete it will be executed.
* You can feed it a String as well.
* Typically, the processor gobbles up and assembles lines into expressions at its own pace. 
* It is more like off-line processing of scripts.

TODO Create Documentation

#### Instrumented Application (REPL server)

The developer embeds a command line server in a standalone application so that the running application can be contacted using a standard telnet session. Instrument an application with a mini server so you can monitor what it is up to.

TODO Create Documentation 

#### Embedded Interpreter 

Create a Scripty interpreter inside the application and invoke the expression evaluator manually. This technique can be used to evaluate command line arguments.

TODO Create Documentation

### Scripty Syntax

TODO Create Documentation

### Available Libraries

The scripty component contains a number of pre-defined command libraries that are at your disposal.

#### Bean Library

Access JavaBeans as a nested directory structure.

#### Debugger Library

Debug commands to interactively debug Scripty programs.

#### Editor Library

Open a GUI text pane to edit some text.

#### Exit Library

Command to exit the application.

#### FileDialog Library

Commands to open a dialog to open or save a file.

#### File Library

UNIX commands to navigate the file system.

#### List Library

Lisp commands to manipulate list structures.

#### Load Library

Load and reload external scripts into the environment.

#### Map Library

Manipulate map data structures.

#### Math Library

Math commands.

#### Pair Library

A pair is a Scripty construct 'key=value'. These commands let you manipulate this data structure.

#### Print Library

print commands.

#### Record Editor Library

A record editor that allows key-value pair editing in a GUI property editor.

#### String Library

String manipulation commands.

### Create a Scripty Command or Macro
#### Defining commands

**Static methods** in a class. Scripty will scan the static methods of the class for annotations and transform each annotated method in a Command or macro.

* Most efficient commands, no extra memory needed the commands are completely stateless. Commands can only share information trough the context.

**Plain objects** with one or more annotated methods. Scripty will scan the instance for both instance methods with annotations and also static methods. Scripty will call the static method or the method on the instance when the command is executed.

* Instances with command methods can have shared state between the command methods. The instance command methods have access to a private memory which is only accessible by the command methods within the same instance. They also have access to the context just like the static method commands.

**Classic command instances**, implementations of the ICommand interface. This type of command can have argument list annotations to do automatic argument checking, but they cannot have parameter mapping specifications since the interface method will be called, and there is no room for negotiation. Classic commands and macros will excel in speed, since there will not be overhead of the dynamic argument handling each time it is called.

Commands can be grouped in libraries. If a command/macro is not specifically associated with a library it will be put in the "global library".

The help annotation contains a description as well as a bundle parameter. If the bundle is provided the description will be interpreted as a key in the corresponding resource bundle. In this way i18n of the help text can be accomplished. The names of the libraries, commands and macro's cannot be internationalized, I prefer the scripts to be independent of the language.

If the command or macro name is not provided, the method name will be used automatically as the name of the command or macro.

```Java
@ScriptyLibrary(name="library-name", onlyStatic=true|false)
@ScriptyHelp(description="...", bundle="...")

@ScriptyCommand(name="command-name")
@ScriptyMacro(name="macro-name")
@ScriptyHelp(description="...", bundle="...")
```

#### Command arguments

Without parameter mappings the command invoker will look at the signature of the command method and provide following resources automatically. All other parameters will receive a null value.

* Object[] will receive the complete parameter list, including the command name in position 0.
* Eval will receive the expression evaluator.
* IContext will receive the current context.
* (CommandRep/MacroRepo)

#### Macro arguments

The first list parameter will receive the expression. All other values will be set to null.

