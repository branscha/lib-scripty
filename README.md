# Scripty Command Language
## Description

Scripty is an embeddable interpreter that provides an extensible command language. Situations where Scripty might be useful:

* Command line parsing during startup of the application.
* Application configuration scripts.
* Command line GUI (graphical or text based) to the embedding application.
* Add automation facilities to the embedding application to enable the user to write scripts against the application model.
* Add scripts to Java annotations to make them more powerful.
* Create builder DSL's.

Furthermore, you have full control over the power of Scripty. By default only the language syntax and control structures are present. You must  augment the power of Scripty by selecting the command libraries explicitly.

## Example - Create a read-eval-print loop

This is only one of the possible ways to embed Scripty in an application. We will use Scripty as the top level REPL, other ways to embed Scripty are described in "[Embedding Scripty](docs/man-embedding-scripty.md)".

This wil create a read-eval-print loop that automatically detects if the application is running on a graphical system or not. If the application is started on a text-only system, a text read-eval-print loop is started, if the application is started in a windowing system a graphical command editing application is started.

```Java
// Create a new REPL application.
public class MyRepl {
    public static void main(String[] args)
    throws ExtensionException {
        // Create the REPL.
        ScriptyAutoRepl repl = new ScriptyAutoRepl();
        // Add the necessary command libraries (or add your own).
        repl.addLibraryClasses(PrintLibrary.class, MathLibrary.class);
        // Start the REPL.
        repl.startLoop();
    }
}
```

Use `./gradlew run` comman to start the minimalistic GUI.

Note that you have to provide a list of command libraries to Scripty, you have complete control over the commands that will be available.

In order to create your own commands that access the business logic you have to create a class and annotate its methods. The class and the methods can be static or not.

```Java
@ScriptyLibrary(name="MyLib", type=ScriptyLibraryType.STATIC)
public class PrintLibrary {
    @ScriptyCommand(name="hi")
    public void method123() {
        System.out.println("Hi there!");
    }
}
```

### Build

The project is based on gradle.

`./gradlew clean build`
	
## Run

Start the minimalistic example GUI:

`./gradlew --console plain repl`

And the text mode repl:

`./gradlew --console plain textrepl`

## Available Libraries

The scripty component comes with a number of pre-defined command libraries that are at your disposal.

### Essential

* [Debugger Library](docs/cmdlib-debugger.md)
* [Exit Command](docs/cmdlib-exit.md)
* [Load Library](docs/cmdlib-scriptloader.md)
* [Map Functions](docs/cmdlib-map.md)
* [Math Functions](docs/cmdlib-math.md)
* [string Functions](docs/cmdlib-string.md)
* [File System Library](docs/cmdlib-filsystem.md)

### Interactive

* [Text editor](docs/cmdlib-editor.md)
* [Record Editor Library](docs/cmdlib-receditor.md)
* [File Dialog Library](docs/cmdlib-filedialog.md)

### Other

* [Pair Functions](docs/cmdlib-pair.md)
* [JavaBean Library](docs/cmdlib-javabean.md)
* [Lisp Functions](docs/cmdlib-lisp.md)

### Using the Command Line
#### Optional Outer Parentheses

Outer parenthesis can be omitted on the command line as a convenience for short commands. Using the parenthesis is off course allowed, it is the only way to enter nested expressions.

In script files the outer parentheses are obligatory.

Example: These two expressions have the same meaning on the command line.

```
> (print Hello World)
Hello World 
> println Hello World
Hello World
```

#### Multi Line Commands

Commands can be spread over multiple lines. This can happen automatically when the command is not complete. You can also ask for this explicitly by terminating the line with a backslash. When you want to quit the multi line command without execution you can issue the break command. It is not really a command since it is directly interpreted by the read-eval-print loop. So you cannot use this command in external scripts.

Example 1: Automatic multi line command.

```
> (println Hello
+ > World
+ > )
Hello World
```

Example 2: Explicit multi line command with backslashes at the end of the line.

```
> println Hello \
+ > World \
+ > )
Hello World
```

Example 3: Terminating a multi line command without execution.

```
> println \
+ > Hello \
+ > break
Canceling the command.
```

#### print

Print stuff on the standard output. Scripty does not automatically print the result of an evaluation on the command line, often you have to use the print command explicitly. It is not uncommon for a command to show something on the standard output and to have something else as the result of the evaluation.

## Advanced Topics

* [Create Command Libraries](docs/man-create-command-libraries.md)
* [Embedding Scripty](docs/man-embedding-scripty.md)
* [Scripty Syntax](docs/man-scripty-syntax.md)