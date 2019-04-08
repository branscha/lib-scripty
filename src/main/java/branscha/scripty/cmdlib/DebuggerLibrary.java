/*******************************************************************************
 * The MIT License
 * Copyright (c) 2012 Bruno Ranschaert
 * lib-scripty
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;
import branscha.scripty.parser.*;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

@ScriptyNamedArgLists(
        std = {
                @ScriptyStdArgList(name = "one argument", fixed = {@ScriptyArg(name = "arg", type = "Any")}),
                @ScriptyStdArgList(name = "no arguments"),
                @ScriptyStdArgList(name = "no arguments + quiet option", named = {@ScriptyArg(name = "quiet", type = "Boolean", optional = true, value = "false")}),
                @ScriptyStdArgList(name = "breakpoint", fixed = {@ScriptyArg(name = "arg", type = "Instance branscha.scripty.parser.EvalTrace$IBreakpoint")}),
                @ScriptyStdArgList(name = "name", fixed = {@ScriptyArg(name = "name", type = "String")}),
                @ScriptyStdArgList(name = "name + bool", fixed = {@ScriptyArg(name = "name", type = "String"), @ScriptyArg(name = "bool", type = "Boolean")}),
                @ScriptyStdArgList(name = "string + name", fixed = {@ScriptyArg(name = "str", type = "String")}, named = {@ScriptyArg(name = "name", type = "String", optional = true, value = "")}),
                @ScriptyStdArgList(name = "posint + name", fixed = {@ScriptyArg(name = "posint", type = "IntegerRange min=0")}, named = {@ScriptyArg(name = "name", type = "String", optional = true, value = "")}),
                @ScriptyStdArgList(name = "obj + name", fixed = {@ScriptyArg(name = "obj", type = "Any")}, named = {@ScriptyArg(name = "name", type = "String", optional = true, value = "")}),
                @ScriptyStdArgList(name = "breakpoint + name", fixed = {@ScriptyArg(name = "bpt", type = "Instance branscha.scripty.parser.EvalTrace$IBreakpoint")}, named = {@ScriptyArg(name = "name", type = "String", optional = true, value = "")})
        },
        var = {
                @ScriptyVarArgList(name = "at least one argument", vararg = @ScriptyArg(name = "arg", type = "Any"), minLength = 1),
                @ScriptyVarArgList(name = "breakpoint* + name", vararg = @ScriptyArg(name = "bpts", type = "Instance branscha.scripty.parser.EvalTrace$IBreakpoint"), minLength = 1, named = {@ScriptyArg(name = "name", type = "String", optional = true, value = "")})
        }
)
public class DebuggerLibrary {
    private static final String MSG_NOTRACE = "No current trace. First start debugging an expression.";
    private static final String MSG_NOBREAKPOINTS = "No current breakpoints.";
    private static final String MSG_NOSTACK = "No current stack.";
    private static final String MSG_NOFRAME = "No current frame.";

    /**
     * The evaluation state for the expression being debugged.
     * In the current implementation there is only one such instance so only one expression
     * can be degugged at a time.
     */
    private EvalTrace trace = null;
    private EvalTrace.BreakpointSet breakpoints = null;
    private int breakpointcounter = 0;

    /**
     * Macro to replace a command with an alternative command and quote the arguments to prevent
     * automatic evaluation. Example (cmd (+ 1 2)) => (cmd-other (quote (+ 1 2))).
     * The replacement command will receive the original argument AST.
     *
     * @param newCmdName The replacement command name.
     * @param expr The complete original expression.
     * @return An S-expression with the new command and quoted argument list.
     * @throws CommandException
     */
    private static List renameAndQuoteArgs(String newCmdName, List<Object> expr)
    throws CommandException {

        // Construct the quoted argument list.
        List<? super Object> quotedArgs = new ArrayList<Object>();
        quotedArgs.add("quote");
        quotedArgs.add(expr.get(1));

        // Construct the replacement expression.
        final List<? super Object> macro = new ArrayList<Object>();
        macro.add(newCmdName);
        macro.add(quotedArgs);

        // Copy the other arguments as-is.
        macro.addAll(expr.subList(2, expr.size()));
        return macro;
    }

    private void checkTrace()
    throws CommandException {
        if (trace == null) throw new CommandException(MSG_NOTRACE);
    }

    private void checkBreakpoints()
    throws CommandException {
        if (breakpoints == null) throw new CommandException(MSG_NOBREAKPOINTS);
    }

    /**
     * Start the debugger with the expression.
     * The debugger is halted in the beginning of evaluation.
     * Starting a new debugging session wil end the previous session if one was active.
     * Macro needed to prevent evaluation of the first argument.
     * Applies following transformation:
     * <br>
     * <code>(dbg-expr &lt;expr>) ==> (dbg-expr-x (quote &lt;expr>))</code>
     *
     * @param aArgs
     * @return
     * @throws CommandException
     */
    @ScriptyMacro(name = "dbg-expr")
    @ScriptyRefArgList(ref = "at least one argument")
    public static List dbgExpr(Object[] aArgs)
    throws CommandException {
        return renameAndQuoteArgs("dbg-expr-x", Arrays.asList(aArgs));
    }

    // Evaluate an expression in the context of the current stack frame.
    // This allows you to examine or modify the frame context during debugging.
    // Use dbg-ctx to print this context and to examine the values.
    //
    // Macro needed to prevent evaluation of the first argument.
    // Applies following transformation:
    // (dbg-eval <expr>) ==> (dbg-eval-x (quote <expr>))
    //
    @ScriptyMacro(name = "dbg-eval")
    @ScriptyRefArgList(ref = "at least one argument")
    public static List dbgEval(Object[] aArgs)
    throws CommandException {
        return renameAndQuoteArgs("dbg-eval-x", Arrays.asList(aArgs));
    }

    // Create a breakpoint that breaks when a condition is met.
    // The condition is evaluated in the context of the current frame.
    // Required arguments:
    // - A conditional expression.
    // Optional arguments:
    // - name: A user name for this breakpoint, otherwise it will be a generated one.
    //
    // A macro is used to prevent evaluation of the condition.
    // In this way the user does not have to quote the conditional expression.
    //
    @ScriptyMacro(name = "bpt-when")
    @ScriptyRefArgList(ref = "at least one argument")
    public static List bptWhen(Object[] aArgs)
    throws CommandException {
        return renameAndQuoteArgs("bpt-when-x", Arrays.asList(aArgs));
    }

    // Internal (effective) command.
    //
    @ScriptyCommand(name = "dbg-expr-x")
    @ScriptyRefArgList(ref = "one argument")
    public void dbgExprInternal(IEval currentEval, IContext context, @ScriptyParam("arg") Object expr) {
        // Halt the previous debug session in order not
        // to clutter up our debugger.
        if (trace != null) trace.terminate();

        // Now create a new one.
        final Eval2 dbgEval = new Eval2();
        dbgEval.setCommandRepo(currentEval.getCommandRepo());
        dbgEval.setMacroRepo(currentEval.getMacroRepo());
        dbgEval.setContext(context);

        // Create a new tracer and save it.
        trace = new EvalTrace(dbgEval, expr);

        // Wire up the breakpoints.
        if (breakpoints == null) breakpoints = trace.getBreakpoints();
        else trace.setBreakpoints(breakpoints);
    }

    private static enum StepType {STEPIN, STEPOVER, STEPOUT, RUN, RUNTORESULT, RUNTOREADY, BACKSTEP}

    // Step into an expression, it takes the smallest step possible.
    //
    @ScriptyCommand(name = "dbg-stepin")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgStepIn(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.STEPIN, aWriter);
    }

    // Step into an expression, it takes the smallest step possible.
    //
    @ScriptyCommand(name = "dbg-step")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgStep(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.STEPIN, aWriter);
    }

    // Run until the current expression on top of the stack is completely  evaluated
    // and then execute the return statement. We return to the previous stack level.
    // It always evaluates the current frame and then goes to the previous frame.
    // This operation always reduces the stack.
    //
    @ScriptyCommand(name = "dbg-stepout")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgStepOut(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.STEPOUT, aWriter);
    }

    // Evaluate the next parameter without stepping trough it or execute the expression.
    // We remain positioned at the same stack level.
    // Use this if you want to evaluate each parameter in turn, without seeing the details of it.
    //
    @ScriptyCommand(name = "dbg-stepover")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgStepOver(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.STEPOVER, aWriter);
    }

    // Take a step back, it might not work as expected because it
    // does not undo bindings, nor other side effects.
    // It could be useful to replay some sequences.
    //
    @ScriptyCommand(name = "dbg-back")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgBack(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.BACKSTEP, aWriter);
    }

    // Keep on running until a result has been produced.
    //
    @ScriptyCommand(name = "dbg-runresult")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgRunResult(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.RUNTORESULT, aWriter);
    }

    // Keep on running until all parameters in the current frame have
    // been evaluated, and the main expression is ready for being executed.
    // Use this if you are not interested in detailed evaluation of the parameters.
    //
    @ScriptyCommand(name = "dbg-runready")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgRunReady(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.RUNTOREADY, aWriter);
    }

    // Keep on running.
    //
    @ScriptyCommand(name = "dbg-run")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean dbgRun(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter)
    throws CommandException {
        return internalStep(StepType.RUN, aWriter);
    }

    private boolean internalStep(StepType op, PrintWriter aWriter)
    throws CommandException {
        checkTrace();
        if (trace.hasMoreSteps()) {
            // Take a step.
            switch (op) {
                case STEPIN:
                    trace.step();
                    break;
                case STEPOVER:
                    trace.stepOver();
                    break;
                case STEPOUT:
                    trace.stepOut();
                    break;
                case RUN:
                    trace.run();
                    break;
                case RUNTORESULT:
                    trace.runToResult();
                    break;
                case RUNTOREADY:
                    trace.runToReady();
                    break;
                case BACKSTEP:
                    trace.backStep();
                    break;
            }

            // Check if there was an exception.
            if (trace.isExcepted()) {
                aWriter.println("An exception occurred during this step. The exception message:\n");
                //noinspection ThrowableResultOfMethodCallIgnored
                aWriter.println(trace.getException().getMessage());
                return Boolean.FALSE;
            }

            if (trace.isBreakpointEncountered()) {
                // Find all the breakpoints that match the current situation (stack).
                // We will compose a message mentioning the labels of all the
                // breakpoints that were triggered.
                final List<EvalTrace.IBreakpoint> lBpts = trace.getBreakpoints().findAllMatchingBreakpoints(trace.getStack());
                StringBuilder lBuilder = new StringBuilder();
                for (EvalTrace.IBreakpoint lBpt : lBpts) lBuilder.append(lBpt.getName());
                aWriter.println("Breakpoint(s) reached: " + lBuilder.toString() + ".");
            }
        }
        else {
            aWriter.println("There are no steps anymore.");
            if (trace.hasResult()) aWriter.println("There is a result though.");
            if (trace.isExcepted()) aWriter.println("The eval was stalled by an exception.");
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    // Print the current stack.
    // Optional arguments:
    // - quiet = true | false*. Prevents writing output, only returns the instance.
    //
    @ScriptyCommand(name = "dbg-stack")
    @ScriptyRefArgList(ref = "no arguments + quiet option")
    public Eval2.EvalStack dbgStack(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter,
                                    @ScriptyParam("quiet") boolean aQuiet)
    throws CommandException {
        checkTrace();
        final Eval2.EvalStack lStack = trace.getStack();
        if (!aQuiet) {
            if (lStack != null) aWriter.print(lStack.toString());
            else {
                aWriter.println("The stack is empty.");
                if (trace.hasResult()) aWriter.println("There is a result though.");
                if (trace.isExcepted()) aWriter.println("The eval was stalled by an exception.");
            }
        }
        return lStack;
    }

    // Print the context of the top of the stack. You can examine all the bindings
    // at that point during the evaluation of the expression. Use dbg-eval to
    // manipulate this context.
    // Optional arguments:
    // - quiet = true | false*. Prevents writing output, only returns the instance.
    //
    @ScriptyCommand(name = "dbg-ctx")
    @ScriptyRefArgList(ref = "no arguments + quiet option")
    public IContext dbgCtx(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter,
                           @ScriptyParam("quiet") boolean aQuiet)
    throws CommandException {
        checkTrace();
        final Eval2.EvalStack lStack = trace.getStack();
        if (lStack == null) throw new CommandException(MSG_NOSTACK);
        final IContext lCtx = lStack.top().getCtx();
        if (!aQuiet) aWriter.print(lCtx.toString());
        return lCtx;
    }

    // Terminate the debugging session.
    //
    @ScriptyCommand(name = "dbg-terminate")
    @ScriptyRefArgList(ref = "no arguments")
    public void dbgTerminate()
    throws CommandException {
        checkTrace();
        // Eat all exceptions at this point, we want to remove the trace.
        try {
            trace.terminate();
        }
        catch (Exception ignored) {
        }
        // Remove the trace from the context.
        trace = null;
    }

    // Raise a CommandException, it is useful to debug certain constructs, or to
    // examine the result of an exception in a certain expression.
    // - Arg: an optional message.
    //
    @ScriptyCommand(name = "dbg-raise")
    @ScriptyRefArgList(ref = "one argument")
    public void dbgRaise(Object[] aArgs)
    throws CommandException {
        String lMsg = "Exception generated by: " + aArgs[0];
        if (aArgs.length > 1 && aArgs[1] != null) lMsg = aArgs[1].toString();
        throw new CommandException(lMsg);
    }

    // Reset the debugger to the beginning of the evaluation.
    // You can restart the debugging of an expression with this.
    // Side effects will not be undone though.
    //
    @ScriptyCommand(name = "dbg-restart")
    @ScriptyRefArgList(ref = "no arguments")
    public void dbgRestart()
    throws CommandException {
        checkTrace();
        if (trace.getStack() == null) throw new CommandException(MSG_NOSTACK);
        trace.reset();
    }

    // Drop the toplevel stackframe.
    // It can be useful to redo the evaluation of a subexpression.
    //
    @ScriptyCommand(name = "dbg-dropframe")
    @ScriptyRefArgList(ref = "no arguments")
    public void dbgDropFrame()
    throws CommandException {
        checkTrace();
        if (trace.getStack() == null) throw new CommandException(MSG_NOSTACK);
        trace.dropFrame();
    }

    // Internal (effective) command.
    //
    @ScriptyCommand(name = "dbg-eval-x")
    @ScriptyRefArgList(ref = "one argument")
    public Object dbgEval(@ScriptyParam("arg") Object aArg, IEval aEval)
    throws CommandException {
        checkTrace();
        Eval2.EvalStack lStack = trace.getStack();
        if (lStack == null) throw new CommandException(MSG_NOSTACK);
        Eval2.StackFrame lFrame = lStack.top();
        if (lFrame == null) throw new CommandException(MSG_NOFRAME);
        final IContext lCtx = lFrame.getCtx();
        return aEval.eval(aArg, lCtx);
    }

    // Check if more steps could be executed in the current debugging session.
    //
    @ScriptyCommand(name = "dbg-moresteps?")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean hasMoreSteps()
    throws CommandException {
        checkTrace();
        return trace.hasMoreSteps();
    }

    // Check if the current debugging session has reached a result.
    //
    @ScriptyCommand(name = "dbg-result?")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean hasResult()
    throws CommandException {
        checkTrace();
        return trace.hasResult();
    }

    // Get the result of the current debugging session if a result has been reached.
    // Otherwise null will be returned, note that the result might be null, in order
    // to maker sure if this is the result or it stands for an empty result use
    // the dbg-result? command.
    //
    @ScriptyCommand(name = "dbg-result")
    @ScriptyRefArgList(ref = "no arguments")
    public Object result()
    throws CommandException {
        checkTrace();
        return trace.getResult();
    }

    // Check if the current debugging session was halted with an exception.
    // If so, the exception will be remembered, you can get it with dbg-exception.
    //
    @ScriptyCommand(name = "dbg-exception?")
    @ScriptyRefArgList(ref = "no arguments")
    public boolean hasException()
    throws CommandException {
        checkTrace();
        return trace.isExcepted();
    }

    // Get the exception that was thrown during the current debugging session if
    // the expression under scrutiny effecively raised an exception.
    // If no exception was raised, null will be returned.
    //
    @ScriptyCommand(name = "dbg-exception")
    @ScriptyRefArgList(ref = "no arguments")
    public Exception exception()
    throws CommandException {
        checkTrace();
        return trace.getException();
    }

    // Add a breakpoint.
    // Required argument: a breakpoint, created with bpt-func, bpt-stack, ...
    // Example: (dbg-addbreakpoint (bpt-func fac))
    //
    @ScriptyCommand(name = "dbg-addbreakpoint")
    @ScriptyRefArgList(ref = "breakpoint")
    public EvalTrace.BreakpointSet addBreakpoint(@ScriptyParam("arg") EvalTrace.IBreakpoint aBpt) {
        // If none was found, we create a new empty one.
        if (breakpoints == null) breakpoints = new EvalTrace.BreakpointSet();
        // At this point, we are sure there is a breakpoint set.
        breakpoints.addBreakpoint(aBpt);
        return breakpoints;
    }

    // List the existing breakpoints known by the debugger.
    //
    @ScriptyCommand(name = "dbg-breakpoints")
    @ScriptyRefArgList(ref = "no arguments + quiet option")
    public EvalTrace.BreakpointSet dbgBreakpoints(@ScriptyBindingParam(value = "*output", unboundException = true) PrintWriter aWriter,
                                                  @ScriptyParam("quiet") boolean aQuiet)
    throws CommandException {
        checkBreakpoints();
        if (!aQuiet && aWriter != null) aWriter.println(breakpoints.toString());
        return breakpoints;
    }

    // Remove a breakpoint.
    // Required argument: the name of the breakpoint.
    // Example (dbg-removebreakpoint bp0)
    //
    @ScriptyCommand(name = "dbg-removebreakpoint")
    @ScriptyRefArgList(ref = "name")
    public EvalTrace.BreakpointSet dbgRemoveBreakpoint(@ScriptyParam("name") String aName)
    throws CommandException {
        checkBreakpoints();
        breakpoints.removeBreakpoint(aName);
        return breakpoints;
    }

    // Enable or disable a named breakpoint.
    // Required arguments:
    // - the breakpoint name.
    // - true | false.
    // Example: (dbg-enablebreakpoint bp0 true)
    //
    @ScriptyCommand(name = "dbg-enablebreakpoint")
    @ScriptyRefArgList(ref = "name + bool")
    public EvalTrace.BreakpointSet dbgEnableBreakpoint(@ScriptyParam("name") String aName, @ScriptyParam("bool") boolean aEnable)
    throws CommandException {
        checkBreakpoints();
        breakpoints.enableBreakpoint(aName, aEnable);
        return breakpoints;
    }

    // Remove all breakpoints from the debugger.
    //
    @ScriptyCommand(name = "dbg-clearbreakpoints")
    @ScriptyRefArgList(ref = "no arguments")
    public EvalTrace.BreakpointSet dbgClearBreakpoints()
    throws CommandException {
        checkBreakpoints();
        breakpoints.removeAllBreakpoints();
        return breakpoints;
    }

    // Create a breakpoint that breaks when a function with a specified
    // name appears on top of the stack.
    // Required arguments:
    // - The function name.
    // Optional arguments:
    // - name: choose a name for this breakpoint, otherwise a name will be generated
    //         of the form bp<x> where <x> is an integer sequence.
    //
    @ScriptyCommand(name = "bpt-func")
    @ScriptyRefArgList(ref = "string + name")
    public EvalTrace.IBreakpoint bptFunc(@ScriptyParam("str") String aFuncName, @ScriptyParam("name") String aBtpName) {
        if (aBtpName.length() <= 0) aBtpName = "bp" + breakpointcounter++;
        return new EvalTrace.BreakpointFunc(aBtpName, aFuncName);
    }

    // Create a breakpoint that breaks when the stack exceeds the specified depth.
    // Required arguments:
    // - the stack size treshold.
    // Optional arguments:
    // - name: A user name for this breakpoint, otherwise it will be a generated one.
    //
    // A macro is used to prevent evaluation of the condition.
    // In this way the user does not have to quote the conditional expression.
    //
    @ScriptyCommand(name = "bpt-stack")
    @ScriptyRefArgList(ref = "posint + name")
    public EvalTrace.IBreakpoint bptStackDepth(@ScriptyParam("posint") Integer aDepth, @ScriptyParam("name") String aBptName) {
        if (aBptName.length() <= 0) aBptName = "bp" + breakpointcounter++;
        return new EvalTrace.BreakpointStackdepth(aBptName, aDepth);
    }

    // Internal (effective) command.
    //
    @ScriptyCommand(name = "bpt-when-x")
    @ScriptyRefArgList(ref = "obj + name")
    public EvalTrace.IBreakpoint bptWhenImpl(@ScriptyParam("obj") Object aExpr, @ScriptyParam("name") String aBptName) {
        if (aBptName.length() <= 0) aBptName = "bp" + breakpointcounter++;
        return new EvalTrace.BreakpointWhen(aBptName, aExpr);
    }

    @ScriptyCommand(name = "bpt-not")
    @ScriptyRefArgList(ref = "breakpoint + name")
    public EvalTrace.IBreakpoint bptNot(@ScriptyParam("bpt") EvalTrace.IBreakpoint aBtp, @ScriptyParam("name") String aBptName) {
        if (aBptName.length() <= 0) aBptName = "bp" + breakpointcounter++;
        return new EvalTrace.BreakpointNot(aBptName, aBtp);
    }


    @ScriptyCommand(name = "bpt-and")
    @ScriptyRefArgList(ref = "breakpoint* + name")
    public EvalTrace.IBreakpoint bptAnd(@ScriptyParam("bpts") Object[] aBpts, @ScriptyParam("name") String aBptName) {
        if (aBptName.length() <= 0) aBptName = "bp" + breakpointcounter++;
        List<EvalTrace.IBreakpoint> lBpts = new LinkedList<EvalTrace.IBreakpoint>();
        for (final Object aBpt : aBpts) lBpts.add((EvalTrace.IBreakpoint) aBpt);
        return new EvalTrace.BreakpointAnd(aBptName, lBpts);
    }

    @ScriptyCommand(name = "bpt-or")
    @ScriptyRefArgList(ref = "breakpoint* + name")
    public EvalTrace.IBreakpoint bptOr(@ScriptyParam("bpts") Object[] aBpts, @ScriptyParam("name") String aBptName) {
        if (aBptName.length() <= 0) aBptName = "bp" + breakpointcounter++;
        List<EvalTrace.IBreakpoint> lBpts = new LinkedList<EvalTrace.IBreakpoint>();
        for (final Object aBpt : aBpts) lBpts.add((EvalTrace.IBreakpoint) aBpt);
        return new EvalTrace.BreakpointOr(aBptName, lBpts);
    }

}
