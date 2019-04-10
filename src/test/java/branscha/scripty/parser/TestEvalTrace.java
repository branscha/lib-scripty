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

import branscha.scripty.ExtensionException;
import branscha.scripty.cmdlib.MathLibrary;
import branscha.scripty.repl.ExtensionRepositoryBuilder;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;


public class TestEvalTrace {

    Parser parser;
    Eval2 eval;
    ExtensionRepositoryBuilder extBldr;

    @Before
    public void setup()
    throws ExtensionException {
        parser = new Parser();
        eval = new Eval2();

        extBldr = new ExtensionRepositoryBuilder();
        extBldr.addLibraryClasses(MathLibrary.class);
        eval.setMacroRepo(extBldr.getMacroRepository());
        eval.setCommandRepo(extBldr.getCommandRepository());
    }

    @Test
    public void testTrace()
    throws CommandException, InterruptedException {

        eval.eval(parser.parseExpression("(defun fac (n) (if (> $n 0) (* $n (fac (- $n 1))) 1))"));
        Object expr = parser.parseExpression("(fac 3)");

        EvalTrace trace = new EvalTrace(eval, expr);
        int stepCounter = 0;
        while (trace.hasMoreSteps()) {
            stepCounter++;
            trace.step();
        }

        assertEquals(true, trace.isTerminated());
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
                "[+, 1, 2]",
                "+\n[+, 1, 2]:0 ~ [null, null, null]",
                "+ ==> +\n[+, 1, 2]:0 ~ [null, null, null]",
                "[+, 1, 2]:1 ~ [+, null, null]",
                "1\n[+, 1, 2]:1 ~ [+, null, null]",
                "1 ==> 1\n[+, 1, 2]:1 ~ [+, null, null]",
                "[+, 1, 2]:2 ~ [+, 1, null]",
                "2\n[+, 1, 2]:2 ~ [+, 1, null]",
                "2 ==> 2\n[+, 1, 2]:2 ~ [+, 1, null]",
                "[+, 1, 2]:3! ~ [+, 1, 2]"
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals(stacks[0], trace.getStack().toString().trim());
        for (int step = 1; step < stacks.length; step++) {
            trace.step();
            assertEquals(stacks[step], trace.getStack().toString().trim());

            assertEquals(false, trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
        trace.step();

        assertEquals(true, trace.isTerminated());
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
        assertEquals("[+, 1, 2]:1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.reset();
        assertEquals("[+, 1, 2]:0 ~ [null, null, null]", trace.getStack().toString().trim());

        assertEquals(false, trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
    }

    @Test
    public void testDropFrame(){
        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);
        for (int i = 0; i < 5; i++) trace.step();
        assertEquals( "1 ==> 1\n[+, 1, 2]:1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.dropFrame();
        assertEquals( "[+, 1, 2]:1 ~ [+, null, null]", trace.getStack().toString().trim());

        trace.step(); trace.step();
        assertEquals( "1 ==> 1\n[+, 1, 2]:1 ~ [+, null, null]", trace.getStack().toString().trim());

        assertEquals(false, trace.isTerminated());
        assertFalse(trace.hasResult());
        assertFalse(trace.isExcepted());
    }

    @Test
    public void testBack() {

        String[] stacks = {
                "[+, 1, 2]:3! ~ [+, 1, 2]",
                "[+, 1, 2]:2 ~ [+, 1, null]",
                "[+, 1, 2]:1 ~ [+, null, null]",
                "[+, 1, 2]:0 ~ [null, null, null]"
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        // Evaluate until the expression is ready to
        // produce a result (but the very last step is not taken.
        trace.runToReady();

        // Now step backwards to the beginning.
        for(int step = 0; step < stacks.length; step++) {

            assertEquals(stacks[step], trace.getStack().toString().trim());

            trace.backStep();

            assertEquals(false, trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
    }

    @Test
    public void testStepOver() {

        String[] stacks = {
                "[+, 1, 2]",
                "[+, 1, 2]:1 ~ [+, null, null]",
                "[+, 1, 2]:2 ~ [+, 1, null]",
                "[+, 1, 2]:3! ~ [+, 1, 2]",
        };

        Object expr = parser.parseExpression("(+ 1 2)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals(stacks[0], trace.getStack().toString().trim());
        for (int step = 1; step < stacks.length; step++) {
            trace.stepOver();
            assertEquals(stacks[step], trace.getStack().toString().trim());

            assertEquals(false, trace.isTerminated());
            assertFalse(trace.hasResult());
            assertFalse(trace.isExcepted());
        }
        trace.stepOver();

        assertEquals(true, trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("3"), lResult);
    }

    @Test
    public void testStepOut() {
        Object expr = parser.parseExpression("(+ (* 2 3) 7)");
        EvalTrace trace = new EvalTrace(eval, expr);

        assertEquals("[+, [*, 2, 3], 7]", trace.getStack().toString().trim());

        // Evaluate until subexpression (* 2 3) is on top.
        trace.stepOver();
        trace.step();
        assertEquals("[*, 2, 3]\n[+, [*, 2, 3], 7]:1 ~ [+, null, null]", trace.getStack().toString().trim());

        // Evaluate the top and substitute in the main expression.
        // => (+ 6 ? )
        trace.stepOut();
        assertEquals("[+, [*, 2, 3], 7]:2 ~ [+, 6, null]", trace.getStack().toString().trim());
    }

    @Test
    public void testRunResult() {
        Object expr = parser.parseExpression("(+ (* 3 4) (* 5 6))");
        EvalTrace trace = new EvalTrace(eval, expr);
        trace.runToResult();

        assertEquals(true, trace.isTerminated());
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

        assertEquals(true, trace.isTerminated());
        assertTrue(trace.hasResult());
        assertFalse(trace.isExcepted());
        Object lResult = trace.getResult();
        assertEquals(new BigDecimal("42"), lResult);
    }
}
