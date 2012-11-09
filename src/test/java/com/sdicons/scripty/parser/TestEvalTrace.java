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
package com.sdicons.scripty.parser;

import com.sdicons.scripty.ExtensionException;
import com.sdicons.scripty.cmdlib.MathLibrary;
import com.sdicons.scripty.repl.ExtensionRepositoryBuilder;
import junit.framework.Assert;
import org.junit.Test;

import java.math.BigDecimal;


public class TestEvalTrace
{
    @Test
    public void testTrace()
    throws CommandException, ExtensionException, InterruptedException
    {
        Parser parser = new Parser();
        Eval2 eval = new Eval2();

        ExtensionRepositoryBuilder lBuilder = new ExtensionRepositoryBuilder();
        lBuilder.addLibraryClasses(MathLibrary.class);
        eval.setMacroRepo(lBuilder.getMacroRepository());
        eval.setCommandRepo(lBuilder.getCommandRepository());

        eval.eval(parser.parseExpression("(defun fac (n) (if (> $n 0) (* $n (fac (- $n 1))) 1))"));
        Object lExpr =  parser.parseExpression("(fac 3)");

        EvalTrace trace = new EvalTrace(eval, lExpr);
        while (trace.hasMoreSteps())
        {
            System.out.println(trace.getStack());
            trace.step();
        }
        System.out.println(trace.getStack());

        Thread.sleep(500);
        Object lResult = trace.getResult();
        Assert.assertEquals(new BigDecimal("6"), lResult);
        //trace.terminate();
    }

}
