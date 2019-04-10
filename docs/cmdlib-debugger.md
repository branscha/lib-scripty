# Debugger Commands

This optional command library contains a complete debugger for Scripty scripts. If you include it the user of the ebmedding application will get full debugging capabilities of the scripts.

## Using the Debug Commands
### Starting a debug session

If we want to debug the expression "2 . 3 + 7" we have to execute following commands:

	> dbg-expr (+ (* 2 3) 7)
	> dbg-step
	> dbg-stack


We first execute `dbg-expr` to indicate what we want to debug. After that we can step trough the execution using the `dbg-step` command. If we want to see what is happening we execute the `dbg-stack` command which shows the current evaluation state.

The result of `dbg-stack` is a stack of expressions, the topmost expression is the most recent one, this expression has to be evaluated before the lower ones. In our example, the + expression will be evaluated before the complete expression.

	+
	[+, [*, 2, 3], 7]:0 ~ [null, null, null]

### Interpreting the stack trace

If we keep on executing (progn (dbg-step) (dbg-stack)) for a while, we will get stack dumps of the form:

	2 ==> 2
	[*, 2, 3]:1 ~ [*, null, null]
	[+, [*, 2, 3], 7]:1 ~ [+, null, null]


This is how we should understand this:

* We are now evaluating string '2', and the result is '2' since numbers evaluate to themselves.
* We were evaluating '2' because in the previous expression on the stack we are evaluating (* 2 3) and we already evaluated the multiplication *.

The list right of the '~' shows the evaluated parts of the expression. In this case we see that the expression [*, 2, 3]:1 is evaluating the first parameter of the expression which is the atomic 2. On the right side we have another representation [*, null, null] which indicates that we already resolved the operation * but we still need to evaluate the two arguments.

An exclamation mark means that the complete frame is ready to be evaluated and be replaced by its result. This is shown in the next example:

    [+, 1, 2]:3! ~ [+, 1, 2]

## Debugger Command Reference

Debug commands to interactively debug Scripty programs.

**dbg-expr**

Start the debugger with the expression. The debugger is halted in the beginning of evaluation. Starting a new debugging session will end the previous session if one was active.

**dbg-eval**

Evaluate an expression in the context of the current stack frame. This allows you to examine or modify the frame context during debugging. Use dbg-ctx to print this context and to examine the values. While you are debugging you can execute stuff in the current context where the debugger is halted.

This command does not change the evaluation of the current execution, it allows to examine the context.

**dbg-step**

Step into an expression, it takes the smallest step possible. It takes the next step in the evaluation of the execution.

**dbg-stepin**

A synonym for dbg-step, it was added to be the inverse for dbg-stepout.

**dbg-back**

Take a step back, it might not work as expected because it does not undo bindings, nor other side effects. It could be useful to replay some sequences.
It jumps from argument to argument without showing the intermediate evaluation, it is the inverse command of `dbg-stepover`.

**dbg-stepout**

Run until the current expression on top of the stack is completely evaluated and then execute the return statement. We return to the previous stack level. It always evaluates the current frame and then goes to the previous frame. This operation always reduces the stack.

**dbg-stepover**

Evaluate the next parameter without stepping trough it or execute the expression. We remain positioned at the same stack level. Use this if you want to evaluate each parameter in turn, without seeing the details of it.
The inverse command is `dbg-back`.

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

Example: 

	(dbg-addbreakpoint (bpt-func fac))

**dbg-breakpoints**

List the existing break points known by the debugger.

**dbg-removebreakpoint**

Remove a break point. Required argument: the name of the break point.

Example:

	(dbg-removebreakpoint bp0)

**dbg-enablebreakpoint**

Enable or disable a named break point. Required arguments:

* The break point name.
* true | false.

Example: 

	(dbg-enablebreakpoint bp0 true)

**dbg-clearbreakpoints**

Remove all break points from the debugger.

**Macro's**

These macros are provided for your convenience. You have to load these explicitly before you can use them by issuing the command (load cp:/dbgutil.lsp).

* **e 'expr**, Start debugging an expression. Don't forget the quote!
* **t**, Terminate the debug session.
* **x**, Stack dump.
* **s**, Step over + stack.
* **b**, Backstep + stack.
* **sover**, Step over (the parameter) + stack.
* **sin**, Step + stack.
* **sout**, Step out (of the expression) + stack.
* **r**, Run until finished.
* **rready**, Run until the parameters of the topmost expression are evaluated and the expression itself can be executed.
* **rresult**, Run until a result is reached.
* **result**, Print the result.
* **ctx**, Print the current context (of the topmost expression).
* **v 'expr**, View an expression, evaluated in the topmost context. Don't forget the quote!
* **restart**, Restart evaluation, start from the beginning.
* **df**, Drop the topmostframe + stack.

### Stepping Granularity

From fastest (less detail) to slowest (most detail):

* dbg-ready/rready: The current frame is ready for evaluation, all parameters have been evaluated.
* dbg-stepover/sover; From argument to argument, we stay on the same frame. 
* dbg-step/s, dbg-stepin/s: Each argument evaluation is shown as well.

