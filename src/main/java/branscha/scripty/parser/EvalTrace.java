/* ******************************************************************************
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
package branscha.scripty.parser;


import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.lang.Thread.sleep;

/**
 * Debugger.
 * Protocol: constructor - step* - getResult.
 */
public class EvalTrace {

    private ModularEval eval;
    private ModularEval.EvalStack stack;

    private Thread worker;

    // A reference is not enough, null could
    // be the result as well.
    private Object result;
    private boolean hasResult = false;
    private boolean halted = false;
    private boolean excepted = false;
    private Exception exception = null;
    private BreakpointSet breakpoints = new BreakpointSet();
    private boolean breakpoint = false;

    // We need the stepcount while stepping with breakpoints.
    // We only break on a breakpoint if we took at least 1 step.
    // Otherwise it would be impossible to continue stepping after a matching breakpoint was encountered.
    private long stepcount = 0;

    private static final String ERR010 = "EvalTrace/010: Expression evaluation was terminated by the user.";
    private static final String ERR020 = "EvalTrace/020: Instruction '%s' can not be performed when evaluation is terminated.";
    private static final String ERR030 = "EvalTrace/030: Instruction '%s' can not be done because the evaluation was terminated by an exception.";

    /**
     * Represents a breakpoint during expression evaluation.
     */
    public interface Breakpoint {
        String INDENT = "    ";

        String getName();

        void setEnabled(boolean aEnabled);

        boolean breakHere(ModularEval.EvalStack stack);

        String toString(String indent);
    }

    /**
     * Keep track of the breakpoints set by the user.
     */
    public static class BreakpointSet {

        private List<Breakpoint> breakpoints = new ArrayList<>();

        public void addBreakpoint(Breakpoint aBpt) {
            breakpoints.add(aBpt);
        }

        public void removeBreakpoint(String aName) {
            for (Breakpoint lBpt : breakpoints) {
                if (lBpt.getName() != null && lBpt.getName().equals(aName)) {
                    breakpoints.remove(lBpt);
                    return;
                }
            }
        }

        public void enableBreakpoint(String aName, boolean aEnable) {
            for (Breakpoint lBpt : breakpoints) {
                if (lBpt.getName() != null && lBpt.getName().equals(aName)) {
                    lBpt.setEnabled(aEnable);
                }
            }
        }

        public void removeAllBreakpoints() {
            breakpoints.clear();
        }

        public List<Breakpoint> findAllMatchingBreakpoints(ModularEval.EvalStack aStack) {
            List<Breakpoint> breakers = new ArrayList<>();
            if (aStack != null) {
                for (Breakpoint lBpt : breakpoints)
                    if (lBpt.breakHere(aStack)) breakers.add(lBpt);
            }
            return breakers;
        }

        public Breakpoint findFirstMatchingBreakpoint(ModularEval.EvalStack aStack) {
            for (Breakpoint lBpt : breakpoints)
                if (lBpt.breakHere(aStack)) return lBpt;
            return null;
        }

        public boolean breakHere(ModularEval.EvalStack aStack) {
            return findFirstMatchingBreakpoint(aStack) != null;
        }

        @Override
        public String toString() {
            return "BreakpointSet{" +
                    "breakpoints=[\n" + breakpoints.stream().map((bpt)->bpt.toString(Breakpoint.INDENT)).collect(Collectors.joining(",\n")) + "]" +
                    '}';
        }
    }

    /**
     * A breakpoint on a function name.
     */
    public static class BreakpointFunc implements Breakpoint {

        private String name;
        private String funcName;
        private boolean enabled = true;

        public BreakpointFunc(String aName, String aFunc) {
            name = aName;
            funcName = aFunc;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack aStack) {
            final ModularEval.StackFrame lFrame = aStack.top();
            final Object lExpr = lFrame.getExpr();

            if (enabled && name != null && lExpr instanceof List) {
                // Fetch the expression that is being evaluated.
                final List lExprLst = (List) lExpr;
                // Only break if the name of the expression is the one we are looking for,
                // and also that the frame slot pointer is at the beginning of the frame,
                // we only want to break the first time an expression is pushed on the stack,
                // not each time it is on top of the stack.
                return (lExprLst.size() > 0) && funcName.equals(lExprLst.get(0)) && (lFrame.getDataptr() == 0);
            }
            return false;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointFunc{" +
                    "name='" + name + '\'' +
                    ", funcName='" + funcName + '\'' +
                    ", enabled=" + enabled +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * A breakpoint on an expression.
     */
    public static class BreakpointWhen implements Breakpoint {

        private String name;
        private Object breakExpression;
        private Eval bptEval = new ClassicEval();
        private boolean enabled = true;

        public BreakpointWhen(String aName, Object aExpr, Eval srcEval) {
            this.name = aName;

            if (aExpr instanceof Token && ((Token) aExpr).isErroneous()) {
                Token token = (Token) aExpr;
                throw new IllegalArgumentException("Expression was not parsed correctly: " + token.getValue());
            }
            this.breakExpression = aExpr;

            // Copy the eval environment in our breakpoint eval.
            bptEval.setMacroRepo(srcEval.getMacroRepo());
            bptEval.setCommandRepo(srcEval.getCommandRepo());
            bptEval.setContext(srcEval.getContext());
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack stack) {
            try {
                return enabled && AbstractEval.boolEval(bptEval.eval(breakExpression, stack.top().getCtx()));
            }
            catch (CommandException e) {
                throw new IllegalArgumentException("Breakpoint expression failure.", e);
            }
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointWhen{" +
                    "name='" + name + '\'' +
                    ", breakExpression='" + Printer.print(breakExpression, false)  + '\'' +
                    ", enabled=" + enabled +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * A breakpoint on a stack depth.
     */
    public static class BreakpointStackdepth implements Breakpoint {

        private String name;
        private boolean enabled = true;
        private int depth;

        public BreakpointStackdepth(String aName, int aDepth) {
            this.name = aName;
            this.depth = aDepth;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack aStack) {
            return enabled && aStack.size() >= depth;
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointStackdepth{" +
                    "name='" + name + '\'' +
                    ", depth=" + depth +
                    ", enabled=" + enabled +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * Breakpoint expression. It inverts an existing breakpoint condition.
     */
    public static class BreakpointNot implements Breakpoint {

        private String name;
        private boolean enabled = true;
        private Breakpoint breakpoint;

        public BreakpointNot(String aName, Breakpoint aBp) {
            this.name = aName;
            this.breakpoint = aBp;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack aStack) {
            return enabled && !(breakpoint.breakHere(aStack));
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointNot{" +
                    "name='" + name + '\'' +
                    ", enabled=" + enabled +
                    ", breakpoint=" + breakpoint +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * Breakpoint expression. It combines a number of breakpoint conditions with and logic.
     */
    public static class BreakpointAnd implements Breakpoint {

        private String name;
        private boolean enabled = true;
        private List<Breakpoint> breakpoints;

        public BreakpointAnd(String aName, List<Breakpoint> aBps) {
            this.name = aName;
            this.breakpoints = aBps;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack aStack) {
            boolean lBreak = enabled;
            for (Breakpoint lBp : breakpoints) {
                lBreak = lBreak && lBp.breakHere(aStack);
                if (!lBreak) return lBreak;
            }
            return lBreak;
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointAnd{" +
                    "name='" + name + '\'' +
                    ", enabled=" + enabled +
                    ", breakpoints=[\n" + breakpoints.stream().map((bpt)->bpt.toString(indent + INDENT)).collect(Collectors.joining(",\n")) + "]" +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * Breakpoint expression. It combines a number of breakpoint conditions with or logic.
     */
    public static class BreakpointOr implements Breakpoint {

        private String name;
        private boolean enabled = true;
        private List<Breakpoint> bps;

        public BreakpointOr(String aName, List<Breakpoint> aBps) {
            this.name = aName;
            this.bps = aBps;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(ModularEval.EvalStack stack) {
            if (!enabled) return false;
            return bps.stream().anyMatch((bp)->bp.breakHere(stack));
        }

        @Override
        public String toString(String indent) {
            return indent + "BreakpointOr{" +
                    "name='" + name + '\'' +
                    ", enabled=" + enabled +
                    ", breakpoints=[\n" + bps.stream().map((bpt)->bpt.toString(indent + INDENT)).collect(Collectors.joining(",\n")) + "]" +
                    '}';
        }

        @Override
        public String toString() {
            return this.toString("");
        }
    }

    /**
     * Give the debugger a signal that it is time
     * to resume work.
     */
    private void wakeUpDebugger(){
        EvalTrace.this.notifyAll();
    }

    /**
     * Pause the eval until it is woken by the debugger.
     */
    private void pauseEval()
    throws CommandException {
        try {
            halted = true;
            EvalTrace.this.wait();
            halted = false;
        }
        catch (InterruptedException e) {
            // If eval is interrupted, it is always the debugger
            // who wants to halt the evaluation process.
            throw new CommandException(ERR010);
        }
    }

    private void stopEval() {
        halted = true;
    }

    /**
     * Wait for eval to finish whatever it is doing and it is halted.
     * The debugger has to wait before accessing the internal data of eval.
     */
    private void waitForEval() {
        try {
            while (!halted) {
                this.wait();
            }
        }
        catch (InterruptedException e) {
            halted = true;
        }
    }

    /**
     * Put eval to work.
     * It is the task of the debugger to put eval to work.
     */
    private void wakeUpEval() {
        this.notifyAll();
    }

    /**
     * Pause the debugger until it gets a notification from eval that something happened.
     */
    private void pauseDebugger() {
        try {
            this.wait();
        }
        catch (InterruptedException e) {
            halted = true;
        }
    }

    private void verifyEvalState(String instruction)  {
        if (worker == null || !worker.isAlive()) throw new IllegalStateException(String.format(ERR020, instruction));
        else if(excepted) throw new IllegalStateException(String.format(ERR030, instruction));
    }

    public EvalTrace(final ModularEval aEval, final Object aExpr) {
        // Keep track of the eval under scrutiny.
        // Add an event listener to it.
        eval = aEval;
        // First remove any  previous listeners.
        // We currently do not allow other listeners.
        eval.removeAllEventListeners();
        // Attach our own listener.
        eval.addEvalListener(new ModularEval.EvalListener() {

            public void finishedEval(ModularEval.EvalEvent aEvent) {
                synchronized (EvalTrace.this) {
                    hasResult = true;
                    excepted = false;

                    // Make data available.
                    result = aEvent.getResult();
                    stack = aEvent.getStack();
                    exception = null;

                    wakeUpDebugger();
                    stopEval();
                }
            }

            public void startEval(ModularEval.EvalEvent aEvent) throws CommandException{
                synchronized (EvalTrace.this) {
                    result = null;
                    hasResult = false;
                    excepted = false;
                    exception = null;
                    // The eval makes its stack available for the debugger.
                    stack = aEvent.getStack();
                    wakeUpDebugger();
                    pauseEval();
                }
            }

            public void stepEvent(ModularEval.EvalEvent aEvent) throws CommandException{
                synchronized (EvalTrace.this) {
                    // Provide the stack for the debugger to examine.
                    stack = aEvent.getStack();
                    wakeUpDebugger();
                    // If there are more steps to take, we let the eval wait a bit.
                    // If this was the last step, we can let the eval worker finish its job
                    // and prepare the result for us.
                    if (stack.hasMoreSteps()) {
                        pauseEval();
                    }
                }
            }

            public void receivedException(ModularEval.EvalEvent aEvent) {
                synchronized (EvalTrace.this) {
                    stack = aEvent.getStack();
                    exception = aEvent.getException();
                    excepted = true;
                    result = null;
                    hasResult = false;
                    //
                    stopEval();
                    wakeUpDebugger();
                }
            }
        });

        // Create a separate suspendable thread to evaluate our expresion in.
        worker = new Thread(() -> {
            try {
                eval.eval(aExpr);
            }
            catch (CommandException e) {
                /*
                    The worker thread should take no action here. By now the eval listener will have been notified
                    about the exception and it is the task of the event listener to make the exception data available.

                    The user of the {@link EvalTrace} should access the exception information using the
                    {@link EvalTrace#isExcepted} and the {@link EvalTrace#getException} methods.

                    It is not the task of the worker thread to print nor log the exception nor the stacktrace
                    because the component is meant to be embedded and it is the responsibility of the embedding
                    class to decide where or on which stream this information should be mad available. It could
                    be written to stdout or stderr, to a file or to a GUI, it is not the responsibility of the worker.
               */
            }
        });

        // Start the eval thread.
        worker.setName("eval/trace");
        worker.setDaemon(true);
        worker.start();
    }

    /**
     * Verify if the evaluator still has steps to take.
     * @return The evaluator can carry on taking steps.
     */
    public synchronized boolean hasMoreSteps() {
        if (worker == null || excepted || !worker.isAlive()) return false;
        waitForEval();
        // This is a difficult piece of code.
        // We halt if there is an exception, this is the easy part.
        // We also halt if the stack has no more steps AND there is not previous stack.
        // There can be multiple stacks if there is a nesting, i.e. a command invokes eval itself.
        // In the nested case, the debugger can go on stepping, no problem.
        return stack != null && !excepted && (stack.hasMoreSteps() || stack.getPrevStack() != null);
    }

    /**
     * Take a single evaluation step.
     */
    public synchronized void step() {
        verifyEvalState("step");
        waitForEval();
        if (!excepted) {
            // Clear the breakpoint flag.
            breakpoint = false;
            // The eval will take the step.
            wakeUpEval();
            // We take a break.
            pauseDebugger();
            stepcount++;
        }
    }

    /**
     * Get the current evaluation stack.
     */
    public synchronized ModularEval.EvalStack getStack() {
        waitForEval();
        return stack;
    }

    /**
     * Obtain the last result produced by the eval under trace.
     * Note that the result can get overwritten by subsequent evaluations, especially by
     * commands using the eval.
     */
    public synchronized Object getResult() {
        waitForEval();
        return result;
    }

    /**
     * Check if the eval under trace has already reached a result.
     * Note that the result could also be null, it is a valid result.
     */
    public synchronized boolean hasResult() {
        // No guard because you always could get the last result for examination,
        // even post mortem.
        waitForEval();
        return hasResult;
    }

    /**
     * Stop tracing. No operation is possible after the debugger has halted.
     * All resources held by the eval being traced will be released.
     * Stopping a tracer is necessary in order not to clutter the memory.
     */
    public void terminate() {
        if (worker == null) {
            // Done deal.
            return;
        }

        if (worker.getState() == Thread.State.TERMINATED) {
            return;
        }
        else {
            worker.interrupt();
            // Join the worker until its finished.
            try {
                worker.join();
            }
            catch (InterruptedException e) {
                // NOP
            }
            finally {
                worker = null;
            }
        }
    }

    /**
     * Check if the evaluation has been halted or not.
     * If it is halted, nothing can be done with it anymore.
     * This is an expensive operation.
     *
     * @return The evaluation is terminated.
     */
    public boolean isTerminated() {
        if (worker == null) {
            return true;
        }

        // It can take a while for the thread to change its state ...
        for (int i = 0; i < 10; i++) {
            if (worker.getState() == Thread.State.TERMINATED) break;
            try {
                sleep(10);
            }
            catch (InterruptedException e) {
               // NOP.
            }
        }
        return (worker.getState() == Thread.State.TERMINATED);
    }

    /**
     * Restart the evaluation of the current expression from the beginning.
     * Ignore all intermediate results obtained so far.
     */
    public synchronized void reset() {
        verifyEvalState("reset");
        waitForEval();
        stack.reset();
    }

    /**
     * Restart the evaluation of the current expression to the beginning.
     * Ignore all intermediate results obtained so far.
     */
    public synchronized void dropFrame() {
        verifyEvalState("dropFrame");
        waitForEval();
        stack.dropFrame();
    }

    /**
     * Step backwards. Note that side effects (context changes) will not be undone.
     * Stepping backwards is like an inverse {@link #stepOver}, it jumps to the previous argument
     * evaluation without showing the details.
     */
    public synchronized void backStep() {
        verifyEvalState("backStep");
        waitForEval();
        final ModularEval.StackFrame lFrame = stack.top();
        if (lFrame.getDataptr() > 0) {
            lFrame.backStep();
        }
        else {
            stack.dropFrame();
        }
    }

    /**
     * Keep on stepping as long as there are more expressions to evaluate and no breakpoint is encountered.
     * This will walk over the results, this can happen when an intermediate command
     * keeps on evaluating expressions in sequence.
     */
    public synchronized void run() {
        // Initialize statistics.
        stepcount = 0;
        breakpoint = false;

        // Take all the steps there are.
        while (hasMoreSteps() && !excepted)
            if (stepcount > 0 && breakpoints.breakHere(stack)) {
                breakpoint = true;
                break;
            }
            else step();
    }

    /**
     * Keep on stepping as long as the expression has not been completely evaluated and no breakpoint is encountered.
     * Note that after the result has been reached, new expressions might be evaluated, so this is
     * not necessarily the end of evaluation. This can happen with intermediary commands.
     */
    public synchronized void runToResult() {
        // Initialize run statistics.
        stepcount = 0;
        breakpoint = false;

        // Take the steps.
        while (!hasResult() && !excepted)
            if (stepcount > 0 && breakpoints.breakHere(stack)) {
                breakpoint = true;
                break;
            }
            else step();
    }

    /**
     * Evaluates the current frame and substitute the result of the evaluation
     * in the parent expression. Stop if a breakpoint is encountered.
     */
    public synchronized void stepOut() {
        // First we run until the current frame has been evaluated.
        // Note that we keep holding on to the same frame for testing,
        // the one that was on the top when this method was called.
        final ModularEval.StackFrame lFrame = getStack().top();
        stepcount = 0;
        breakpoint = false;

        // Take some steps.
        while (!lFrame.isEvaluated() && !excepted)
            if (stepcount > 0 && breakpoints.breakHere(stack)) {
                breakpoint = true;
                break;
            }
            else step();
        // Now we take a single step to step out of the frame.
        if (this.hasMoreSteps())
            if (stepcount > 0 && breakpoints.breakHere(stack)) breakpoint = true;
            else step();
    }

    /**
     * Run until all the arguments in the current frame have been evaluated and the complete
     * frame can be evaluated and  no breakpoint is encountered in the process.
     */
    public synchronized void runToReady() {
        // We run until the current frame has been evaluated.
        // Note that we keep holding on to the same frame for testing,
        // the one that was on the top when this method was called.
        final ModularEval.StackFrame lFrame = getStack().top();
        stepcount = 0;
        breakpoint = false;

        // Take some steps.

        while (
            // If there is no handler, the data slots will not have been initialized (this is the task of the handler)
            // and the slot pointer will not be a reliable metric.
                (lFrame.getHandler() == null) ||
                        ((lFrame.getDataptr() < lFrame.getData().length) && !excepted))
            if (stepcount > 0 && breakpoints.breakHere(stack)) {
                breakpoint = true;
                break;
            }
            else step();
    }

    /**
     * It works like a {@link #step()} but it jumps from argument to argument
     * without showing the intermediate results. The inverse of stepOver is {@link #backStep()}.
     */
    public synchronized void stepOver() {
        if (excepted) return;

        final ModularEval.StackFrame lFrame = getStack().top();
        final int lStartSlot = lFrame.getDataptr();

        // Initialize run statistics.
        stepcount = 0;
        breakpoint = false;

        // Take some steps.
        if (lFrame.isEvaluated()) {
            if (stepcount > 0 && breakpoints.breakHere(stack)) breakpoint = true;
            else step();
        }
        else {
            while (
                // If there is not yet a handler, the frame slots will not have
                // been initialized and the datapointer will not be a reliable metric.
                    lFrame.getHandler() == null ||
                            (!lFrame.isEvaluated() && !excepted && (lFrame.getDataptr() <= lStartSlot)))
                if (stepcount > 0 && breakpoints.breakHere(stack)) {
                    breakpoint = true;
                    break;
                }
                else step();
        }
    }

    public synchronized boolean isExcepted() {
        waitForEval();
        return excepted;
    }

    public synchronized Exception getException() {
        waitForEval();
        return exception;
    }

    public synchronized BreakpointSet getBreakpoints() {
        return breakpoints;
    }

    public synchronized void setBreakpoints(BreakpointSet breakpoints) {
        this.breakpoints = breakpoints;
    }

    public boolean isBreakpointEncountered() {
        return breakpoint;
    }
}
