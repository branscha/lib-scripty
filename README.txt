Scripty, a Command Language for Embedding
=========================================

License: LGPL

Uses:
 * Command line parsing. The command line can be a complete expression. Especially pair expressions are useful here.
 * Config files, run when the app starts, so the user can add customisations here.
 * Console window, where the user can engage with command dialog.
 * Application scripts.

By adding a number of commands the programmer has a scripting language that works directly with concepts from the
business domain.

Characteristics:
 * Optimised for interactive use by the end user and easy extesibility by the programmer.
 * Programming in the small.
 * Scripts in the business domain.

Admitted, a command line interface is not a sexy interface and it is not for the average user. But it allows
the programmer to create a toolbox very fast, targeted at power users.

It includes a complete debugging system for your scripts, even across command invocations.

A large number of optional command libraries are included, you can choose to omit them from your application,
it depends on the functionality you want to provide and the level of security you want to maintain.

Implementing your own commands is straightforward. There is a system to help semi automatic argument checking.
You don't haves to use it, it is optional. The context can be used for inter command communication. Values not meant
for the end user conventionally start with an asterisks. Examples are the *input, *output and *error streams and
also the *frame handle.

Not implemented
 * 'defmacro' is not implemented at this point. You can define macros in java, but not in scripty.
   It would not be too difficult to add this feature.

Build instructions:
 * It is a Maven project, there are no dependencies.
 * There are several entry points:
   - Repl can be started to get a plain command line interface.
   - ExampleLoop contains another entry point.
   - Interpreter can be used to execute scripts in the file system.
   - ReplServer can be used to access a remote server.

Licenses
- Nano HTTPD from Jarno Elonen <elonen@iki.fi>.
  A modified BSD License.