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
package branscha.scripty.parser;


import java.util.ArrayList;
import java.util.List;

import static java.lang.Thread.sleep;

/**
 * Debugger.
 * Protocol: consructor - step* - getResult.
 * - Resultaat *moet* opgehaald worden, de eval wacht op een {@link #step()} of een {@link #getResult()}.
 */
public class EvalTrace {

    private Eval2 eval;
    private Eval2.EvalStack stack;

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
    public interface IBreakpoint {
        String getName();

        void setEnabled(boolean aEnabled);

        boolean breakHere(Eval2.EvalStack aStack);
    }

    /**
     * Keep track of the breakpoints set by the user.
     */
    public static class BreakpointSet {

        private List<IBreakpoint> breakpoints = new ArrayList<>();

        public void addBreakpoint(IBreakpoint aBpt) {
            breakpoints.add(aBpt);
        }

        public void removeBreakpoint(String aName) {
            for (IBreakpoint lBpt : breakpoints) {
                if (lBpt.getName() != null && lBpt.getName().equals(aName)) {
                    breakpoints.remove(lBpt);
                    return;
                }
            }
        }

        public void enableBreakpoint(String aName, boolean aEnable) {
            for (IBreakpoint lBpt : breakpoints) {
                if (lBpt.getName() != null && lBpt.getName().equals(aName)) {
                    lBpt.setEnabled(aEnable);
                }
            }
        }

        public void removeAllBreakpoints() {
            breakpoints.clear();
        }

        public List<IBreakpoint> findAllMatchingBreakpoints(Eval2.EvalStack aStack) {
            List<IBreakpoint> lResult = new ArrayList<>();
            if (aStack != null) {
                for (IBreakpoint lBpt : breakpoints)
                    if (lBpt.breakHere(aStack)) lResult.add(lBpt);
            }
            return lResult;
        }

        public IBreakpoint findFirstMatchingBreakpoint(Eval2.EvalStack aStack) {
            for (IBreakpoint lBpt : breakpoints)
                if (lBpt.breakHere(aStack)) return lBpt;
            return null;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            return findFirstMatchingBreakpoint(aStack) != null;
        }

        @Override
        public String toString() {
            final StringBuilder builder = new StringBuilder();
            for (IBreakpoint lBpt : breakpoints) {
                builder.append(lBpt.toString()).append("\n");
            }
            return builder.toString();
        }
    }

    /**
     * A breakpoint on a function name.
     */
    public static class BreakpointFunc implements EvalTrace.IBreakpoint {

        private String name;
        private String func;
        private boolean enabled = true;

        public BreakpointFunc(String aName, String aFunc) {
            name = aName;
            func = aFunc;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            final Eval2.StackFrame lFrame = aStack.top();
            final Object lExpr = lFrame.getExpr();

            if (enabled && name != null && lExpr instanceof List) {
                // Fetch the expression that is being evaluated.
                final List lExprLst = (List) lExpr;
                // Only break if the name of the expression is the one we are looking for,
                // and also that the frame slot pointer is at the beginning of the frame,
                // we only want to break the first time an expression is pushed on the stack,
                // not each time it is on top of the stack.
                return (lExprLst.size() > 0) && func.equals(lExprLst.get(0)) && (lFrame.getDataptr() == 0);
            }
            return false;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return this.name + ", " + "name break: '" + this.func + "'" + (enabled ? ", enabled" : ", paused");
        }
    }

    /**
     * A breakpoint on an expression.
     */
    public static class BreakpointWhen implements IBreakpoint {

        private String name;
        private Object lWhenExpr;
        private IEval eval = new Eval();
        private boolean enabled = true;

        public BreakpointWhen(String aName, Object aExpr) {
            this.name = aName;
            this.lWhenExpr = aExpr;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            try {
                return enabled && AbstractEval.boolEval(eval.eval(lWhenExpr, aStack.top().getCtx()));
            }
            catch (CommandException e) {
                return false;
            }
        }

        @Override
        public String toString() {
            return this.name + ", " + "expression break: '" + lWhenExpr.toString() + "'" + (enabled ? ", enabled" : ", paused");
        }
    }

    /**
     * A breakpoint on a stack depth.
     */
    public static class BreakpointStackdepth implements IBreakpoint {

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

        public boolean breakHere(Eval2.EvalStack aStack) {
            return enabled && aStack.size() >= depth;
        }

        @Override
        public String toString() {
            return this.name + ", " + "stack depth break: '" + depth + "'" + (enabled ? ", enabled" : ", paused");
        }
    }

    /**
     * Breakpoint expression. It inverts an existing breakpoint condition.
     */
    public static class BreakpointNot implements IBreakpoint {

        private String name;
        private boolean enabled = true;
        private IBreakpoint bp;

        public BreakpointNot(String aName, IBreakpoint aBp) {
            this.name = aName;
            this.bp = aBp;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            return enabled && !(bp.breakHere(aStack));
        }

        @Override
        public String toString() {
            return this.name + ", " + "not-composite" + (enabled ? ", enabled" : ", paused") +
                    "\n\t" + bp.toString().replace("\n\t", "\n\t\t");
        }
    }

    /**
     * Breakpoint expression. It combines a number of breakpoint conditions with and logic.
     */
    public static class BreakpointAnd implements IBreakpoint {

        private String name;
        private boolean enabled = true;
        private List<IBreakpoint> bps;

        public BreakpointAnd(String aName, List<IBreakpoint> aBps) {
            this.name = aName;
            this.bps = aBps;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            boolean lBreak = enabled;
            for (IBreakpoint lBp : bps) {
                lBreak = lBreak && lBp.breakHere(aStack);
                if (!lBreak) return lBreak;
            }
            return lBreak;
        }

        @Override
        public String toString() {
            StringBuilder lBuilder = new StringBuilder();
            lBuilder.append(this.name).append(", ").append("and-composite").append(enabled ? ", enabled" : ", paused");
            for (IBreakpoint lBp : bps)
                lBuilder.append("\n\t * ").append(lBp.toString().replace("\n\t", "\n\t\t"));
            return lBuilder.toString();
        }
    }

    /**
     * Breakpoint expression. It combines a number of breakpoint conditions with or logic.
     */
    public static class BreakpointOr implements IBreakpoint {

        private String name;
        private boolean enabled = true;
        private List<IBreakpoint> bps;

        public BreakpointOr(String aName, List<IBreakpoint> aBps) {
            this.name = aName;
            this.bps = aBps;
        }

        public String getName() {
            return name;
        }

        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }

        public boolean breakHere(Eval2.EvalStack aStack) {
            if (!enabled) return false;
            boolean lBreak = false;
            for (IBreakpoint lBp : bps) {
                lBreak = lBp.breakHere(aStack);
                if (lBreak) return lBreak;
            }
            return lBreak;
        }

        @Override
        public String toString() {
            StringBuilder lBuilder = new StringBuilder();
            lBuilder.append(this.name).append(", ").append("or-composite").append(enabled ? ", enabled" : ", paused");
            for (IBreakpoint lBp : bps)
                lBuilder.append("\n\t * ").append(lBp.toString().replace("\n\t", "\n\t\t"));
            return lBuilder.toString();
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

    public EvalTrace(final Eval2 aEval, final Object aExpr) {
        // Keep track of the eval under scrutiny.
        // Add an event listener to it.
        eval = aEval;
        eval.addEvalListener(new Eval2.EvalListener() {

            public void finishedEval(Eval2.EvalEvent aEvent) {
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

            public void startEval(Eval2.EvalEvent aEvent) throws CommandException{
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

            public void stepEvent(Eval2.EvalEvent aEvent) throws CommandException{
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

            public void receivedException(Eval2.EvalEvent aEvent) {
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
                // The eval will have notified any listeners.
                // Our EvalTrace will have received the exception event and should have
                // taken appropriate action.
                // We can let this thread die.
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
    public synchronized Eval2.EvalStack getStack() {
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
     * Restart the evaluation of the current expression to the beginning.
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
     */
    public synchronized void backStep() {
        verifyEvalState("backStep");
        waitForEval();
        final Eval2.StackFrame lFrame = stack.top();
        if (lFrame.getDataptr() > 0) {
            lFrame.backStep();
        }
        else {
            stack.dropFrame();
        }
    }

    /**
     * Keep on stepping as long as there are more expressions to evaluate.
     * This will walk over the results, this can happen when an intermediate command
     * keeps on evaluating expressions in sequence.
     */
    public synchronized void run() {
        // Initialize statistics.
        stepcount = 0;
        breakpoint = false;

        // Take some steps.
        while (hasMoreSteps() && !excepted)
            if (stepcount > 0 && breakpoints.breakHere(stack)) {
                breakpoint = true;
                break;
            }
            else step();
    }

    /**
     * Keep on stepping as long as the expression has not been completely evaluated.
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

    public synchronized void stepOut() {
        // First we run until the current frame has been evaluated.
        // Note that we keep holding on to the same frame for testing,
        // the one that was on the top when this method was called.
        final Eval2.StackFrame lFrame = getStack().top();
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

    public synchronized void runToReady() {
        // We run until the current frame has been evaluated.
        // Note that we keep holding on to the same frame for testing,
        // the one that was on the top when this method was called.
        final Eval2.StackFrame lFrame = getStack().top();
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

    public synchronized void stepOver() {
        if (excepted) return;

        final Eval2.StackFrame lFrame = getStack().top();
        final int lStartSlot = lFrame.getDataptr();

        // Initialize runstatistics.
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

    public BreakpointSet getBreakpoints() {
        return breakpoints;
    }

    public void setBreakpoints(BreakpointSet breakpoints) {
        this.breakpoints = breakpoints;
    }

    public boolean isBreakpointEncountered() {
        return breakpoint;
    }
}
