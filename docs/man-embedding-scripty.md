# Different Ways to Embed Scripty

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