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
import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.TreeSet;

import static java.lang.Thread.sleep;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;


public class TestEvalTrace {

    Parser parser;
    Eval2 eval;
    ExtensionRepositoryBuilder extBldr;

    @Before
    public void setup() throws ExtensionException{
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
        for(int i = 0; i < 10; i++) {
            trace.step();
        }

        assertFalse(trace.isTerminated());
        trace.terminate();

        assertTrue(trace.isTerminated());
        assertFalse(trace.hasResult());
        assertTrue(trace.isExcepted());
        assertThat(trace.getException().getMessage(), containsString("EvalTrace/010"));
    }
}
