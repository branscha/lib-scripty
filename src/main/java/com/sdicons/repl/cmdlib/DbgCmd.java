/*
 * Scripty Programming Language
 * Copyright (C) 2010-2011 Bruno Ranschaert, S.D.I.-Consulting BVBA
 * http://www.sdi-consulting.be
 * mailto://info@sdi-consulting.be
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package com.sdicons.repl.cmdlib;

import com.sdicons.repl.parser.*;
import com.sdicons.repl.spec.args.*;
import com.sdicons.repl.spec.type.*;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class DbgCmd
extends AbstractCommand
{
    public static enum DbgCmdOp {
            // Debug commands.
            dbgStepIn, dbgStepOver, dbgStepOut,
            dbgRun, dbgRunToResult, dbgRunToReady, dbgStack, dbgExpr, dbgTerminate,
            dbgRaise, dbgBackStep, dbgRestart, dbgDropframe, dbgEval,
            dbgHasMoreSteps, dbgHasResult, dbgResult, dbgHasException, dbgException, dbgCtx,
            // Breakpoint manipulation.
            dbgAddBreakpoint, dbgBreakpoints, dbgRemoveBreakpoint, dbgClearBreakpoints, dbgEnableBreakpoint,
            // Breakpoint creation.
            bptFunc, bptWhen, bptStackDepth, bptAnd, bptOr, bptNot}

    // Used in order to generate new unique breakpoint names.
    private static long breakpointcounter = 0;

    // TYPES
    ////////
    private static final ITypeSpec TBREAKPOINT = new InstanceType(EvalTrace.IBreakpoint.class, "breakpoint", false);
    private static final ITypeSpec TEXPR = new InstanceType(Object.class, "expression", false);
    // ARGS
    ////////
    private static final NamedArg NQUIET = new NamedArg("quiet", BooleanType.BOOLEAN_TYPE, Boolean.FALSE, true);
    private static final NamedArg NNAME = new NamedArg("name", StringType.STRING_TYPE, null, true);
    private static final FixedArg FFUNC = new FixedArg(StringType.STRING_TYPE);
    private static final FixedArg FNAME = new FixedArg(StringType.STRING_TYPE);
    private static final FixedArg FBOOL = new FixedArg(BooleanType.BOOLEAN_TYPE);
    private static final FixedArg FDEPTH = new FixedArg(new IntegerRangeType(1, Integer.MAX_VALUE));
    private static final FixedArg FBREAKPOINT = new FixedArg(TBREAKPOINT);
    private static final FixedArg FEXPR = new FixedArg(TEXPR);
    private static final VarArg   VBREAKPOINT = new VarArg(TBREAKPOINT);
    // PARAMETER LISTS
    //////////////////
    private static final IArgList NOARG_NQUIET_ = new StdArgList(new FixedArg[]{}, new OptionalArg[]{}, new NamedArg[]{NQUIET});
    private static final IArgList FEXPR_NNAME_ = new StdArgList(new FixedArg[]{FEXPR}, new OptionalArg[]{}, new NamedArg[]{NNAME});
    private static final IArgList FFUNC_NNAME_ = new StdArgList(new FixedArg[]{FFUNC}, new OptionalArg[]{}, new NamedArg[]{NNAME});
    private static final IArgList FDEPTH_NNAME_ = new StdArgList(new FixedArg[]{FDEPTH}, new OptionalArg[]{}, new NamedArg[]{NNAME});
    private static final IArgList FNAME_ = new StdArgList(new FixedArg[]{FNAME}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FNAME_FBOOL_ = new StdArgList(new FixedArg[]{FNAME, FBOOL}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FBREAKPOINT_ = new StdArgList(new FixedArg[]{FBREAKPOINT}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FBREAKPOINT_NNAME_ = new StdArgList(new FixedArg[]{FBREAKPOINT}, new OptionalArg[]{}, new NamedArg[]{NNAME});
    private static final IArgList VBREAKPOINT_NNAME_ = new VarArgList(new FixedArg[]{}, VBREAKPOINT, 1, -1, new NamedArg[]{NNAME});

    @SuppressWarnings("unchecked")
    private static List quoteMacro(String extName, String intName, List anExpr)
    throws CommandException
    {
        if(anExpr.size() < 2) throw new CommandException(String.format("ERROR: '%s' expects at least an expression as an argument.", extName));
        List lMacro = new ArrayList();
        lMacro.add(intName);

        List lQuoted = new ArrayList();
        // Quote the first argument.
        lQuoted.add("quote");
        lQuoted.add(anExpr.get(1));
        lMacro.add(lQuoted);
        // Copy the other arguments as-is.
        lMacro.addAll(anExpr.subList(2, anExpr.size()));
        return lMacro;
    }

    public static void registerCommands(IRegistry aReg)
    {
        // Start the debugger with the expression.
        // The debugger is halted in the beginning of evaluation.
        // Starting a new debugging session wil end the previous session if one was active.
        //
        // Macro needed to prevent evaluation of the first argument.
        // Applies following transformation:
        // (dbg-expr <expr>) ==> (dbg-expr-x (quote <expr>))
        aReg.registerMacro("dbg-expr", new AbstractMacro()
        {
            public Object transform(List anExpr)
            throws CommandException
            {
                return DbgCmd.quoteMacro("dbg-expr", "dbg-expr-x", anExpr);
            }
        });
        // Internal (effective) command.
        aReg.registerCommand("dbg-expr-x", new DbgCmd(DbgCmdOp.dbgExpr));

        // Evaluate an expression in the context of the top level stack frame.
        // This allows you to examine or modify the frame context during debugging.
        // Use dbg-ctx to print this context and to examine the values.
        //
        // Macro needed to prevent evaluation of the first argument.
        // Applies following transformation:
        // (dbg-eval <expr>) ==> (dbg-eval-x (quote <expr>))
        aReg.registerMacro("dbg-eval", new AbstractMacro()
        {
            public Object transform(List anExpr)
            throws CommandException
            {
                return DbgCmd.quoteMacro("dbg-eval", "dbg-eval-x", anExpr);
            }
        });
        // Internal (effective) command.
        aReg.registerCommand("dbg-eval-x", new DbgCmd(DbgCmdOp.dbgEval));

        // Step into an expression, it takes the smallest step possible.
        aReg.registerCommand("dbg-step", new DbgCmd(DbgCmdOp.dbgStepIn));
        // A synonym for step.
        aReg.registerCommand("dbg-stepin", new DbgCmd(DbgCmdOp.dbgStepIn));

        // Take a step back, it might not work as expected because it
        // does not undo bindings, nor other side effects.
        // It could be useful to replay some sequences.
        aReg.registerCommand("dbg-back", new DbgCmd(DbgCmdOp.dbgBackStep));
        // Run until the current expression on top of the stack is completely  evaluated
        // and then execute the return statement. We return to the previous stack level.
        // It always evaluates the current frame and then goes to the previous frame.
        // This operation always reduces the stack.
        aReg.registerCommand("dbg-stepout", new DbgCmd(DbgCmdOp.dbgStepOut));
        // Evaluate the next parameter without stepping trough it or execute the expression.
        // We remain positioned at the same stack level.
        // Use this if you want to evaluate each parameter in turn, without seeing the details of it.
        aReg.registerCommand("dbg-stepover", new DbgCmd(DbgCmdOp.dbgStepOver));
        // Print the current stack.
        // Optional arguments:
        // - quiet = true | false*. Prevents writing output, only returns the instance.
        aReg.registerCommand("dbg-stack", new DbgCmd(DbgCmdOp.dbgStack));
        // Terminate the debugging session.
        aReg.registerCommand("dbg-terminate", new DbgCmd(DbgCmdOp.dbgTerminate));
        // Raise a CommandException, it is useful to debug certain constructs, or to
        // examine the result of an exception in a certain expression.
        // - Arg: an optional message.
        aReg.registerCommand("dbg-raise", new DbgCmd(DbgCmdOp.dbgRaise));
        // Keep on running.
        aReg.registerCommand("dbg-run", new DbgCmd(DbgCmdOp.dbgRun));
        // Keep on running until a result has been produced.
        aReg.registerCommand("dbg-runresult", new DbgCmd(DbgCmdOp.dbgRunToResult));
        // Keep on running until all parameters in the current frame have
        // been evaluated, and the main expression is ready for being executed.
        // Use this if you are not interested in detailed evaluation of the parameters.
        aReg.registerCommand("dbg-runready", new DbgCmd(DbgCmdOp.dbgRunToReady));
        // Reset the debugger to the beginning of the evaluation.
        // You can restart the debugging of an expression with this.
        // Side effects will not be undone though.
        aReg.registerCommand("dbg-restart", new DbgCmd(DbgCmdOp.dbgRestart));
        // Drop the toplevel stackframe.
        // It can be useful to redo the evaluation of a subexpression.
        aReg.registerCommand("dbg-dropframe", new DbgCmd(DbgCmdOp.dbgDropframe));
        // Check if more steps could be executed in the current debugging session.
        aReg.registerCommand("dbg-moresteps?", new DbgCmd(DbgCmdOp.dbgHasMoreSteps));
        // Check if the current debugging session has reached a result.s
        aReg.registerCommand("dbg-result?", new DbgCmd(DbgCmdOp.dbgHasResult));
        // Check if the current debugging session was halted with an exception.
        // If so, the exception will be remembered, you can get it with dbg-exception.
        aReg.registerCommand("dbg-exception?", new DbgCmd(DbgCmdOp.dbgHasException));
        // Get the result of the current debugging session if a result has been reached.
        // Otherwise null will be returned, note that the result might be null, in order
        // to maker sure if this is the result or it stands for an empty result use
        // the dbg-result? command.
        aReg.registerCommand("dbg-result", new DbgCmd(DbgCmdOp.dbgResult));
        // Get the exception that was thrown during the current debugging session if
        // the expression under scrutiny effecively raised an exception.
        // If no exception was raised, null will be returned.
        aReg.registerCommand("dbg-exception", new DbgCmd(DbgCmdOp.dbgException));
        // Print the context of the top of the stack. You can examine all the bindings
        // at that point during the evaluation of the expression. Use dbg-eval to
        // manipulate this context.
        // Optional arguments:
        // - quiet = true | false*. Prevents writing output, only returns the instance.
        aReg.registerCommand("dbg-ctx", new DbgCmd(DbgCmdOp.dbgCtx));

        // Create a breakpoint that breaks when a function with a specified
        // name appears on top of the stack.
        // Required arguments:
        // - The function name.
        // Optional arguments:
        // - name: choose a name for this breakpoint, otherwise a name will be generated
        //         of the form bp<x> where <x> is an integer sequence.
        aReg.registerCommand("bpt-func", new DbgCmd(DbgCmdOp.bptFunc));
        // Internal (effective) command.
        aReg.registerCommand("bpt-when-x", new DbgCmd(DbgCmdOp.bptWhen));
        // Create a breakpoint that breaks when a condition is met.
        // The condition is evaluated in the context of the current frame.
        // Required arguments:
        // - A conditional expression.
        // Optional arguments:
        // - name: A user name for this breakpoint, otherwise it will be a generated one.
        //
        // A macro is used to prevent evaluation of the condition.
        // In this way the user does not have to quote the conditional expression.
        aReg.registerMacro("bpt-when", new AbstractMacro()
        {
            public Object transform(List anExpr)
            throws CommandException
            {
                return DbgCmd.quoteMacro("bpt-when", "bpt-when-x", anExpr);
            }
        });
        // Create a breakpoint that breaks when the stack exceeds the specified depth.
        // Required arguments:
        // - the stack size treshold.
        // Optional arguments:
        // - name: A user name for this breakpoint, otherwise it will be a generated one.
        //
        // A macro is used to prevent evaluation of the condition.
        // In this way the user does not have to quote the conditional expression.
        aReg.registerCommand("bpt-stack", new DbgCmd(DbgCmdOp.bptStackDepth));
        aReg.registerCommand("bpt-and", new DbgCmd(DbgCmdOp.bptAnd));
        aReg.registerCommand("bpt-or", new DbgCmd(DbgCmdOp.bptOr));
        aReg.registerCommand("bpt-not", new DbgCmd(DbgCmdOp.bptNot));
        // Add a breakpoint.
        // Required argument: a breakpoint, created with bpt-func, bpt-stack, ...
        // Example: (dbg-addbreakpoint (bpt-func fac))
        aReg.registerCommand("dbg-addbreakpoint", new DbgCmd(DbgCmdOp.dbgAddBreakpoint));
        // List the existing breakpoints known by the debugger.
        aReg.registerCommand("dbg-breakpoints", new DbgCmd(DbgCmdOp.dbgBreakpoints));
        // Remove a breakpoint.
        // Required argument: the name of the breakpoint.
        // Example (dbg-removebreakpoint bp0)
        aReg.registerCommand("dbg-removebreakpoint", new DbgCmd(DbgCmdOp.dbgRemoveBreakpoint));
        // Enable or disable a named breakpoint.
        // Required arguments:
        // - the breakpoint name.
        // - true | false.
        // Example: (dbg-enablebreakpoint bp0 true)
        aReg.registerCommand("dbg-enablebreakpoint", new DbgCmd(DbgCmdOp.dbgEnableBreakpoint));
        // Remove all breakpoints from the debugger.
        aReg.registerCommand("dbg-clearbreakpoints", new DbgCmd(DbgCmdOp.dbgClearBreakpoints));
    }

    private DbgCmdOp op;

    private static final String TRACE = "*dbg";
    private static final String BREAKPOINTS = "*bpt";
    private static final String MSG_NOTRACE = "No current trace. First start debugging an expression.";
    private static final String MSG_NOBREAKPOINTS = "No current breakpoints.";
    private static final String MSG_NOSTACK = "No current stack.";
    private static final String MSG_NOFRAME = "No current frame.";

    public DbgCmd(DbgCmdOp op)
    {
        super();
        this.op = op;
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        // Get the printer from the context.
        final Object lWriterObj = aCtx.getBinding("*output");
        PrintWriter lWriter = null;
        if (lWriterObj instanceof PrintWriter) lWriter = (PrintWriter) lWriterObj;

        try
        {
            switch(op)
            {
                case dbgExpr:
                {
                    // Halt the previous debug session in order not
                    // to clutter up our debugger.
                    final EvalTrace lOldTrc = getCurrTrace(aCtx);
                    if(lOldTrc != null) lOldTrc.terminate();

                    // Now create a new one.
                    final Eval2 lEval = new Eval2();
                    Map<String, ICommand> lCmds = aEval.dumpCommands();
                    for(String lComKey : lCmds.keySet()) lEval.registerCommand(lComKey, lCmds.get(lComKey));

                    Map<String, IMacro> lMacros = aEval.dumpMacros();
                    for(String lMacroKey: lMacros.keySet()) lEval.registerMacro(lMacroKey, lMacros.get(lMacroKey));

                    lEval.setContext(aCtx);

                    // Create a new tracer and save it.
                    final EvalTrace lNewTrc = new EvalTrace(lEval, aArgs[1]);
                    aCtx.getRootContext().defBinding(TRACE, lNewTrc);

                    // Wire up the breakpoints.
                    final EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    if(lBpts == null) aCtx.getRootContext().defBinding(BREAKPOINTS, lNewTrc.getBreakpoints());
                    else lNewTrc.setBreakpoints(lBpts);
                    break;
                }
                case dbgStepIn:
                case dbgStepOver:
                case dbgStepOut:
                case dbgRun:
                case dbgRunToResult:
                case dbgRunToReady:
                case dbgBackStep:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    if(lTrace.hasMoreSteps())
                    {
                        // Take a step.
                        if(op == DbgCmdOp.dbgStepIn) lTrace.step();
                        else if(op == DbgCmdOp.dbgStepOver) lTrace.stepOver();
                        else if(op == DbgCmdOp.dbgStepOut) lTrace.stepOut();
                        else if(op == DbgCmdOp.dbgRun) lTrace.run();
                        else if(op == DbgCmdOp.dbgRunToResult) lTrace.runToResult();
                        else if(op == DbgCmdOp.dbgRunToReady) lTrace.runToReady();
                        else if(op == DbgCmdOp.dbgBackStep) lTrace.backStep();

                        // Check if there was an exception.
                        if(lTrace.isExcepted())
                        {
                            if(lWriter != null) lWriter.println("An exception occurred during this step. The exception message:\n");
                            if(lWriter != null) lWriter.println(lTrace.getException().getMessage());
                            return Boolean.FALSE;
                        }

                        if(lTrace.isBreakpointEncountered())
                        {
                            // Find all the breakpoints that match the current situation (stack).
                            // We will compose a message mentioning the labels of all the
                            // breakpoints that were triggered.
                            final List<EvalTrace.IBreakpoint> lBpts = lTrace.getBreakpoints().findAllMatchingBreakpoints(lTrace.getStack());
                            StringBuilder lBuilder = new StringBuilder();
                            for(EvalTrace.IBreakpoint lBpt: lBpts) lBuilder.append(lBpt.getName());
                            if(lWriter != null) lWriter.println("Breakpoint(s) reached: " + lBuilder.toString() + ".");
                        }
                    }
                    else
                    {
                        if(lWriter != null) lWriter.println("There are no steps anymore.");
                        if(lTrace.hasResult()) if(lWriter != null) lWriter.println("There is a result though.");
                        if(lTrace.isExcepted())if(lWriter != null) lWriter.println("The eval was stalled by an exception.");
                        return Boolean.FALSE;
                    }
                    return Boolean.TRUE;
                }
                case dbgStack:
                {
                    final Object[] lArgs = NOARG_NQUIET_.guard(aArgs, aCtx);
                    boolean lQuiet = (Boolean) lArgs[1];

                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    final Eval2.EvalStack lStack = lTrace.getStack();
                    if(!lQuiet)
                    {
                        if(lStack != null) if(lWriter != null) lWriter.print(lStack.toString());
                        else
                        {
                            if(lWriter != null) lWriter.println("The stack is empty.");
                            if(lTrace.hasResult()) if(lWriter != null) lWriter.println("There is a result though.");
                            if(lTrace.isExcepted()) if(lWriter != null) lWriter.println("The eval was stalled by an exception.");
                        }
                    }
                    return lStack;
                }
                case dbgCtx:
                {
                    final Object[] lArgs = NOARG_NQUIET_.guard(aArgs, aCtx);
                    boolean lQuiet = (Boolean) lArgs[1];

                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    final Eval2.EvalStack lStack = lTrace.getStack();
                    if(lStack == null) throw new CommandException(MSG_NOSTACK);
                    final IContext lCtx = lStack.top().getCtx();
                    if(!lQuiet && lWriter != null) lWriter.print(lCtx.toString());
                    return lCtx;
                }
                case dbgTerminate:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    // Eat all exceptions at this point, we want to remove the trace.
                    try {lTrace.terminate();}catch(Exception ignored){}
                    // Remove the trace from the context.
                    aCtx.removeBinding(TRACE);
                    break;
                }
                case dbgRaise:
                    String lMsg = "Exception generated by: " + aArgs[0];
                    if(aArgs.length > 1 && aArgs[1]!= null) lMsg = aArgs[1].toString();
                    throw new CommandException(lMsg);
                case dbgRestart:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    if(lTrace.getStack() == null) throw new CommandException(MSG_NOSTACK);
                    lTrace.reset();
                    break;
                }
                case dbgDropframe:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    if(lTrace.getStack() == null) throw new CommandException(MSG_NOSTACK);
                    lTrace.dropFrame();
                    break;
                }
                case dbgEval:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    Eval2.EvalStack lStack = lTrace.getStack();
                    if(lStack == null) throw new CommandException(MSG_NOSTACK);
                    Eval2.StackFrame lFrame = lStack.top();
                    if(lFrame == null) throw new CommandException(MSG_NOFRAME);
                    final IContext lCtx = lFrame.getCtx();
                    return aEval.eval(aArgs[1], lCtx);
                }
                case dbgHasMoreSteps:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    return lTrace.hasMoreSteps();
                }
                case dbgHasResult:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    return lTrace.hasResult();
                }
                case dbgResult:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    return lTrace.getResult();
                }
                case dbgHasException:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    return lTrace.isExcepted();
                }
                case dbgException:
                {
                    final EvalTrace lTrace = getCurrTrace(aCtx);
                    if(lTrace == null) throw new CommandException(MSG_NOTRACE);
                    return lTrace.getException();
                }
                case dbgAddBreakpoint:
                {
                    final Object[] lArgs = FBREAKPOINT_.guard(aArgs, aCtx);
                    EvalTrace.IBreakpoint lBp = (EvalTrace.IBreakpoint)  lArgs[1];

                    // Fetch the current breakpoint set.
                    EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    // If none was found, we create a new empty one.
                    if(lBpts == null)
                    {
                        lBpts = new EvalTrace.BreakpointSet();
                        aCtx.getRootContext().defBinding(BREAKPOINTS, lBpts);
                    }
                    // At this point, we are sure there is a breakpoint set.
                    lBpts.addBreakpoint(lBp);
                    return lBpts;
                }
                case dbgBreakpoints:
                {
                    // Fetch the current breakpoint set.
                    EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    if(lBpts == null) throw new CommandException(MSG_NOBREAKPOINTS);

                    final Object[] lArgs = NOARG_NQUIET_.guard(aArgs, aCtx);
                    boolean lQuiet = (Boolean) lArgs[1];

                    if(!lQuiet && lWriter != null) lWriter.println(lBpts.toString());

                    return lBpts;
                }
                case dbgRemoveBreakpoint:
                {
                    // Fetch the current breakpoint set.
                    EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    if(lBpts == null) throw new CommandException(MSG_NOBREAKPOINTS);

                    final Object[] lArgs = FNAME_.guard(aArgs, aCtx);
                    String lName  = (String) lArgs[1];
                    lBpts.removeBreakpoint(lName);
                    return lBpts;
                }
                case dbgEnableBreakpoint:
                {
                    // Fetch the current breakpoint set.
                    EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    if(lBpts == null) throw new CommandException(MSG_NOBREAKPOINTS);

                    final Object[] lArgs = FNAME_FBOOL_.guard(aArgs, aCtx);
                    String lName  = (String) lArgs[1];
                    boolean lEnable = (Boolean) lArgs[2];

                    lBpts.enableBreakpoint(lName, lEnable);
                    return lBpts;

                }
                case dbgClearBreakpoints:
                {
                    // Fetch the current breakpoint set.
                    EvalTrace.BreakpointSet lBpts = getCurrBreakpoints(aCtx);
                    if(lBpts == null) throw new CommandException(MSG_NOBREAKPOINTS);
                    lBpts.removeAllBreakpoints();
                    return lBpts;
                }
                case bptFunc:
                {
                    final Object[] lArgs = FFUNC_NNAME_.guard(aArgs, aCtx);
                    String lName = (String) lArgs[2];
                    String lFuncName = (String) lArgs[1];

                    // Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    return new EvalTrace.BreakpointFunc(lName, lFuncName);
                }
                case bptStackDepth:
                {
                    final Object[] lArgs = FDEPTH_NNAME_.guard(aArgs, aCtx);
                    String lName = (String) lArgs[2];
                    int lDepth = (Integer) lArgs[1];

                    // Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    return new EvalTrace.BreakpointStackdepth(lName, lDepth);
                }
                case bptWhen:
                {
                    final Object[] lArgs = FEXPR_NNAME_.guard(aArgs, aCtx);
                    Object lExpr = lArgs[1];
                    String lName = (String) lArgs[2];

                    //  Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    return new EvalTrace.BreakpointWhen(lName, lExpr);
                }
                case bptNot:
                {
                    final Object[] lArgs = FBREAKPOINT_NNAME_.guard(aArgs, aCtx);
                    EvalTrace.IBreakpoint lBp = (EvalTrace.IBreakpoint)  lArgs[1];
                    String lName = (String) lArgs[2];
                    // Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    EvalTrace.IBreakpoint lNotBp = new EvalTrace.BreakpointNot(lName, lBp);
                    return lNotBp;
                }
                case bptAnd:
                {
                    final Object[] lArgs = VBREAKPOINT_NNAME_.guard(aArgs, aCtx);
                    String lName = (String) lArgs[1];
                    // Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    List<EvalTrace.IBreakpoint> lBpts = new LinkedList<EvalTrace.IBreakpoint>();
                    for(int i = 2; i < (lArgs.length); i++) lBpts.add((EvalTrace.IBreakpoint)lArgs[i]);
                    EvalTrace.IBreakpoint lBptAnd = new EvalTrace.BreakpointAnd(lName, lBpts);
                    return lBptAnd;
                }
                case bptOr:
                {
                    final Object[] lArgs = VBREAKPOINT_NNAME_.guard(aArgs, aCtx);
                    String lName = (String) lArgs[1];
                    // Generate a new one.
                    if(lName == null) lName = "bp" + breakpointcounter++;
                    List<EvalTrace.IBreakpoint> lBpts = new LinkedList<EvalTrace.IBreakpoint>();
                    for(int i = 2; i < (lArgs.length); i++) lBpts.add((EvalTrace.IBreakpoint)lArgs[i]);
                    EvalTrace.IBreakpoint lBptOr = new EvalTrace.BreakpointOr(lName, lBpts);
                    return lBptOr;
                }
                default:
                    throw new CommandException("ERROR: Internal DbgCmd error, unhandled op-code.");
            }
        }
        catch (ArgSpecException e)
        {
            throw new CommandException(String.format("ERROR: Command '%s' argument error.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
        return null;
    }

    private static EvalTrace getCurrTrace(IContext aCtx)
    {
        if(aCtx.isBound(TRACE))
        {
            Object lCand = aCtx.getBinding(TRACE);
            if(lCand instanceof EvalTrace)
            {
                return (EvalTrace)lCand;
            }
        }
        return null;
    }

    private static EvalTrace.BreakpointSet getCurrBreakpoints(IContext aCtx)
    {
        if(aCtx.isBound(BREAKPOINTS))
        {
            Object lCand = aCtx.getBinding(BREAKPOINTS);
            if(lCand instanceof EvalTrace.BreakpointSet)
            {
                return (EvalTrace.BreakpointSet)lCand;
            }
        }
        return null;
    }
}