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

import branscha.scripty.ExtensionException;
import branscha.scripty.cmdlib.MathLibrary;
import branscha.scripty.repl.ExtensionRepositoryBuilder;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;


public class TestEvalTrace {

    private static final String FUN_FAC = "(defun fac (n) (if (> $n 0) (* $n (fac (- $n 1))) 1))";

    private static final String FAC_STACK_8 = "(if (> $n 0) (* $n (fac (- $n 1))) 1)\n" +
            "(fac (- $n 1)):2! ~ [fac, 8]\n" +
            "(* $n (fac (- $n 1))):2 ~ [*, 9, null]\n" +
            "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
            "(fac (- $n 1)):2! ~ [fac, 9]\n" +
            "(* $n (fac (- $n 1))):2 ~ [*, 10, null]\n" +
            "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
            "(fac 10):2! ~ [fac, 10]";

    private Parser parser;
    private Eval2 eval;

    @Before
    public void setup()
    throws ExtensionException {
        parser = new Parser();
        eval = new Eval2();

        ExtensionRepositoryBuilder extBldr = new ExtensionRepositoryBuilder();
        extBldr.addLibraryClasses(MathLibrary.class);
        eval.setMacroRepo(extBldr.getMacroRepository());
        eval.setCommandRepo(extBldr.getCommandRepository());
    }

    @Test
    public void testTrace()
    throws CommandException {

        eval.eval(parser.parseExpression(FUN_FAC));
        Object expr = parser.parseExpression("(fac 3)");

        EvalTrace trace = new EvalTrace(eval, expr);
        int stepCounter = 0;
        while (trace.hasMoreSteps()) {
            stepCounter++;
            trace.step();
        }

        assertTrue(trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals(147, stepCounter);
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("6"), lResult);
    }

    @Test
    public void testInterrupt() {
        // Construct endless loop.
        Object expr = parser.parseExpression("(while true (+ 1 1))");
        EvalTrace trace = new EvalTrace(eval, expr);

        // Take some steps (but not all).
        for (int i = 0; i < 10; i++) {
            trace.step();
        }

        assertFalse(trace.isTerminated());
        trace.terminate();

        assertTrue(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertTrue(trace.isExcepted());
        assertThat(trace.getException().getMessage(), containsString("EvalTrace/010"));
    }

    @Test
    public void testStack() {
        String[] stacks = {
                "(+ 1 2)",
                "+\n(+ 1 2):0 ~ [null, null, null]",
                "+ ==> +\n(+ 1 2):0 ~ [null, null, null]",
                "(+ 1 2):1 ~ [+, null, null]",
                "1\n(+ 1 2):1 ~ [+, null, null]",
                "1 ==> 1\n(+ 1 2):1 ~ [+, null, null]",
                "(+ 1 2):2 ~ [+, 1, null]",
                "2\n(+ 1 2):2 ~ [+, 1, null]",
                "2 ==> 2\n(+ 1 2):2 ~ [+, 1, null]",
                "(+ 1 2):3! ~ [+, 1, 2]"
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals(stacks[0], trace.getStack().toString().trim());
        for (int step = 1; step < stacks.length; step++) {
            trace.step();
            assertEquals(stacks[step], trace.getStack().toString().trim());

            assertFalse(trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
        trace.step();

        assertTrue(trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("3"), lResult);
    }

    @Test
    public void testReset() {
        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);
        for (int i = 0; i < 3; i++) trace.step();
        assertEquals("(+ 1 2):1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.reset();
        assertEquals("(+ 1 2):0 ~ [null, null, null]", trace.getStack().toString().trim());

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
    }

    @Test
    public void testDropFrame(){
        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);
        for (int i = 0; i < 5; i++) trace.step();
        assertEquals( "1 ==> 1\n(+ 1 2):1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.dropFrame();
        assertEquals( "(+ 1 2):1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.step(); trace.step();
        assertEquals( "1 ==> 1\n(+ 1 2):1 ~ [+, null, null]", trace.getStack().toString().trim());

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
    }

    @Test
    public void testBack() {

        String[] stacks = {
                "(+ 1 2):3! ~ [+, 1, 2]",
                "(+ 1 2):2 ~ [+, 1, null]",
                "(+ 1 2):1 ~ [+, null, null]",
                "(+ 1 2):0 ~ [null, null, null]"
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        // Evaluate until the expression is ready to
        // produce a result (but the very last step is not taken.
        trace.runToReady();

        // Now step backwards to the beginning.
        for (String stack : stacks) {

            assertEquals(stack, trace.getStack().toString().trim());

            trace.backStep();

            assertFalse(trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
    }

    @Test
    public void testStepOver() {

        String[] stacks = {
                "(+ 1 2)",
                "(+ 1 2):1 ~ [+, null, null]",
                "(+ 1 2):2 ~ [+, 1, null]",
                "(+ 1 2):3! ~ [+, 1, 2]",
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals(stacks[0], trace.getStack().toString().trim());
        for (int step = 1; step < stacks.length; step++) {
            trace.stepOver();
            assertEquals(stacks[step], trace.getStack().toString().trim());

            assertFalse(trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
        trace.stepOver();

        assertTrue(trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("3"), lResult);
    }

    @Test
    public void testStepOut() {
        Object expr = parser.parseExpression("(+ (* 2 3) 7)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals("(+ (* 2 3) 7)", trace.getStack().toString().trim());

        // Evaluate until subexpression (* 2 3) is on top.
        trace.stepOver();
        trace.step();
        assertEquals("(* 2 3)\n(+ (* 2 3) 7):1 ~ [+, null, null]", trace.getStack().toString().trim());

        // Evaluate the top and substitute in the main expression.
        // => (+ 6 ? )
        trace.stepOut();
        assertEquals("(+ (* 2 3) 7):2 ~ [+, 6, null]", trace.getStack().toString().trim());
    }

    @Test
    public void testRunResult() {
        Object expr = parser.parseExpression("(+ (* 3 4) (* 5 6))");
        EvalTrace trace = new EvalTrace(eval, expr);
        trace.runToResult();

        assertTrue(trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("42"), lResult);
    }

    @Test
    public void testRun() {
        Object expr = parser.parseExpression("(+ (* 3 4) (* 5 6))");
        EvalTrace trace = new EvalTrace(eval, expr);
        trace.run();

        assertTrue(trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("42"), lResult);
    }

    @Test
    public void testStackDepthBpt()
    throws CommandException {

        eval.eval(parser.parseExpression(FUN_FAC));

        Object expr = parser.parseExpression("(fac 100000000)");
        EvalTrace trace = new EvalTrace(eval, expr);
        EvalTrace.Breakpoint bpt = new EvalTrace.BreakpointStackdepth("sdepth", 10);
        trace.getBreakpoints().addBreakpoint(bpt);

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals(">\n" +
                "(> $n 0):0 ~ [null, null, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):0 ~ [null, null]\n" +
                "(fac (- $n 1)):2! ~ [fac, 99999998]\n" +
                "(* $n (fac (- $n 1))):2 ~ [*, 99999999, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac (- $n 1)):2! ~ [fac, 99999999]\n" +
                "(* $n (fac (- $n 1))):2 ~ [*, 100000000, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac 100000000):2! ~ [fac, 100000000]", trace.getStack().toString().trim());

        trace.terminate();
        assertTrue(trace.isTerminated());
    }

    @Test
    public void testFuncBtp()
    throws CommandException {

        eval.eval(parser.parseExpression(FUN_FAC));

        Object expr = parser.parseExpression("(fac 100)");
        EvalTrace trace = new EvalTrace(eval, expr);
        EvalTrace.Breakpoint bpt = new EvalTrace.BreakpointFunc("func", "*");
        trace.getBreakpoints().addBreakpoint(bpt);

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals("(* $n (fac (- $n 1)))\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac 100):2! ~ [fac, 100]", trace.getStack().toString().trim());

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals("(* $n (fac (- $n 1)))\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac (- $n 1)):2! ~ [fac, 99]\n" +
                "(* $n (fac (- $n 1))):2 ~ [*, 100, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac 100):2! ~ [fac, 100]", trace.getStack().toString().trim());

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals("(* $n (fac (- $n 1)))\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac (- $n 1)):2! ~ [fac, 98]\n" +
                "(* $n (fac (- $n 1))):2 ~ [*, 99, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac (- $n 1)):2! ~ [fac, 99]\n" +
                "(* $n (fac (- $n 1))):2 ~ [*, 100, null]\n" +
                "(if (> $n 0) (* $n (fac (- $n 1))) 1):1 ~ [true, null]\n" +
                "(fac 100):2! ~ [fac, 100]", trace.getStack().toString().trim());

        trace.terminate();
        assertTrue(trace.isTerminated());
    }

    @Test
    public void testWhenBtp()
    throws CommandException {

        eval.eval(parser.parseExpression(FUN_FAC));

        Object expr = parser.parseExpression("(fac 10)");
        EvalTrace trace = new EvalTrace(eval, expr);
        EvalTrace.Breakpoint bpt = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(if $n (< $n 9) false)"), eval);
        trace.getBreakpoints().addBreakpoint(bpt);

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals(FAC_STACK_8, trace.getStack().toString().trim());

        trace.terminate();
        assertTrue(trace.isTerminated());
    }

    @Test
    public void testAndBpt() {

        EvalTrace.Breakpoint bpt1 = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(eq $color red)"), eval);
        EvalTrace.Breakpoint bpt2 = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(eq $size large)"), eval);
        EvalTrace.Breakpoint bptAnd = new EvalTrace.BreakpointAnd("and", Arrays.asList(bpt1, bpt2));

        // No red nor large -> run to end.
        Object expr = parser.parseExpression("(progn (defvar color=black) (defvar size=small))");
        EvalTrace trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptAnd);

        trace.run();
        assertTrue(trace.isTerminated());
        assertFalse((trace.isExcepted()));
        eval.getContext().removeBinding("color");
        eval.getContext().removeBinding("size");

        // No red only large -> run to end.
        expr = parser.parseExpression("(progn (defvar color=black) (defvar size=large))");
        trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptAnd);

        trace.run();
        assertTrue(trace.isTerminated());
        assertFalse((trace.isExcepted()));
        eval.getContext().removeBinding("color");
        eval.getContext().removeBinding("size");

        // Only red not large -> run to end.
        expr = parser.parseExpression("(progn (defvar color=red) (defvar size=small))");
        trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptAnd);

        trace.run();
        assertTrue(trace.isTerminated());
        assertFalse((trace.isExcepted()));
        eval.getContext().removeBinding("color");
        eval.getContext().removeBinding("size");

        // BOth conditions are met - the breakpoint halts eval.
        expr = parser.parseExpression("(progn (defvar color=red) (defvar size=large) )");
        trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptAnd);

        trace.run();
        assertFalse(trace.isTerminated());
        assertFalse((trace.isExcepted()));
    }

    @Test
    public void testBtpNot()
    throws CommandException {

        eval.eval(parser.parseExpression(FUN_FAC));

        Object expr = parser.parseExpression("(fac 10)");
        EvalTrace trace = new EvalTrace(eval, expr);
        EvalTrace.Breakpoint bpt = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(if $n (>= $n 9) true)"), eval);
        EvalTrace.Breakpoint bptNot = new EvalTrace.BreakpointNot("not", bpt);
        trace.getBreakpoints().addBreakpoint(bptNot);

        trace.run();

        assertFalse(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
        assertEquals(FAC_STACK_8, trace.getStack().toString().trim());

        trace.terminate();
        assertTrue(trace.isTerminated());
    }

    @Test
    public void testBptOr() {

        EvalTrace.Breakpoint bpt1 = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(eq $color red)"), eval);
        EvalTrace.Breakpoint bpt2 = new EvalTrace.BreakpointWhen("when", parser.parseExpression("(eq $color blue)"), eval);
        EvalTrace.Breakpoint bptOr = new EvalTrace.BreakpointOr("or", Arrays.asList(bpt1, bpt2));

        // No red nor blue -> run to end.
        Object expr = parser.parseExpression("(progn (defvar color=none) (set color=black) (set color=green) (set color=cyan ) (set color=yellow))");
        EvalTrace trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptOr);

        trace.run();
        assertTrue(trace.isTerminated());
        assertFalse((trace.isExcepted()));

        // red is there -> breakpoint hit.
        expr = parser.parseExpression("(progn (defvar color=none) (set color=black) (set color=green) (set color=red ) (set color=yellow))");
        trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptOr);

        trace.run();
        assertFalse(trace.isTerminated());
        assertFalse((trace.isExcepted()));
        assertEquals("(set color=red):2! ~ [color, red] ==> red\n" +
                "(progn (defvar color=none) (set color=black) (set color=green) (set color=red) (set color=yellow)):4 ~ [progn, none, black, green, null, null]", trace.getStack().toString().trim());
        trace.terminate();

        // blue is there -> breakpoint hit.
        expr = parser.parseExpression("(progn (defvar color=none)  (set color=black) (set color=green) (set color=blue ) (set color=yellow))");
        trace = new EvalTrace(eval, expr);
        trace.getBreakpoints().addBreakpoint(bptOr);

        trace.run();
        assertFalse(trace.isTerminated());
        assertFalse((trace.isExcepted()));
        assertEquals("progn\n" +
                "(progn (defvar color=none) (set color=black) (set color=green) (set color=blue) (set color=yellow)):0 ~ [null, null, null, null, null, null]", trace.getStack().toString().trim());
        trace.terminate();
    }

    @Test
    public void testBtpToString() {
        EvalTrace.Breakpoint bpt1 = new EvalTrace.BreakpointWhen("when-red", parser.parseExpression("(eq $color red)"), eval);
        assertEquals("BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true}", bpt1.toString());

        EvalTrace.Breakpoint bpt2 = new EvalTrace.BreakpointWhen("when-blue", parser.parseExpression("(eq $color blue)"), eval);
        assertEquals("BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}", bpt2.toString());

        EvalTrace.Breakpoint bptOr = new EvalTrace.BreakpointOr("or", Arrays.asList(bpt1, bpt2));
        assertEquals("BreakpointOr{name='or', enabled=true, breakpoints=[\n" +
                "    BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "    BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]}", bptOr.toString());

        EvalTrace.Breakpoint bptAnd = new EvalTrace.BreakpointAnd("and", Arrays.asList(bpt1, bpt2));
        assertEquals("BreakpointAnd{name='and', enabled=true, breakpoints=[\n" +
                "    BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "    BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]}", bptAnd.toString());

        EvalTrace.Breakpoint bptFunc = new EvalTrace.BreakpointFunc("func", "*");
        assertEquals("BreakpointFunc{name='func', funcName='*', enabled=true}", bptFunc.toString());

        EvalTrace.Breakpoint bptDepth = new EvalTrace.BreakpointStackdepth("sdepth", 10);
        assertEquals("BreakpointStackdepth{name='sdepth', depth=10, enabled=true}", bptDepth.toString());

        EvalTrace.Breakpoint bptNot = new EvalTrace.BreakpointNot("not", bptFunc);
        assertEquals("BreakpointNot{name='not', enabled=true, breakpoint=BreakpointFunc{name='func', funcName='*', enabled=true}}", bptNot.toString());

        EvalTrace.BreakpointSet bptSet = new EvalTrace.BreakpointSet();
        bptSet.addBreakpoint(bpt1);
        bptSet.addBreakpoint(bpt2);
        bptSet.addBreakpoint(bptOr);
        bptSet.addBreakpoint(bptAnd);
        bptSet.addBreakpoint(bptFunc);
        bptSet.addBreakpoint(bptDepth);
        bptSet.addBreakpoint(bptNot);

        assertEquals("BreakpointSet{breakpoints=[\n" +
                "    BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "    BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true},\n" +
                "    BreakpointOr{name='or', enabled=true, breakpoints=[\n" +
                "        BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "        BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]},\n" +
                "    BreakpointAnd{name='and', enabled=true, breakpoints=[\n" +
                "        BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "        BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]},\n" +
                "    BreakpointFunc{name='func', funcName='*', enabled=true},\n" +
                "    BreakpointStackdepth{name='sdepth', depth=10, enabled=true},\n" +
                "    BreakpointNot{name='not', enabled=true, breakpoint=BreakpointFunc{name='func', funcName='*', enabled=true}}]}", bptSet.toString());

        bptSet.removeBreakpoint("func");
        bptSet.removeBreakpoint("sdepth");
        bptSet.removeBreakpoint("when-blue");

        assertEquals("BreakpointSet{breakpoints=[\n" +
                "    BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "    BreakpointOr{name='or', enabled=true, breakpoints=[\n" +
                "        BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "        BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]},\n" +
                "    BreakpointAnd{name='and', enabled=true, breakpoints=[\n" +
                "        BreakpointWhen{name='when-red', breakExpression='(eq $color red)', enabled=true},\n" +
                "        BreakpointWhen{name='when-blue', breakExpression='(eq $color blue)', enabled=true}]},\n" +
                "    BreakpointNot{name='not', enabled=true, breakpoint=BreakpointFunc{name='func', funcName='*', enabled=true}}]}", bptSet.toString());
    }

    @Test
    public void testPauseBpt()
    throws CommandException {

        eval.eval(parser.parseExpression("(defun countit ()  (progn  (defvar counter=0) (while (< $counter 100) (set counter=(+ $counter 1)))))"));
        Object expr = parser.parseExpression("(countit)");
        EvalTrace trace = new EvalTrace(eval, expr);

        EvalTrace.Breakpoint bptEven = new EvalTrace.BreakpointWhen("when-even", parser.parseExpression("(and $counter (not (rem $counter 2)))"), eval);
        EvalTrace.Breakpoint bptOld = new EvalTrace.BreakpointWhen("when-50", parser.parseExpression("(and $counter (>= $counter 50))"), eval);

        trace.getBreakpoints().addBreakpoint(bptEven);
        trace.getBreakpoints().addBreakpoint(bptOld);

        trace.run();
        assertFalse(trace.isTerminated());
        assertTrue(trace.isBreakpointEncountered());
        assertEquals(new BigDecimal(2), eval.getContext().getBinding("counter"));

        bptEven.setEnabled(false);

        trace.run();
        assertFalse(trace.isTerminated());
        assertTrue(trace.isBreakpointEncountered());
        assertEquals(new BigDecimal(50), eval.getContext().getBinding("counter"));
        eval.getContext().setBinding("counter", new BigDecimal(51));

        bptOld.setEnabled(false);
        bptEven.setEnabled(true);

        trace.runToResult();
        assertFalse(trace.isTerminated());
        assertTrue(trace.isBreakpointEncountered());
        assertEquals(new BigDecimal(52), eval.getContext().getBinding("counter"));

        bptEven.setEnabled(false);

        trace.run();
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("100"), lResult);
    }
}


