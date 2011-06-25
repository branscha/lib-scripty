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

package com.sdicons.repl;

import java.math.BigDecimal;

import com.sdicons.repl.cmdlib.MathCmd;
import com.sdicons.repl.cmdlib.PrintCmd;
import com.sdicons.repl.parser.CommandException;
import com.sdicons.repl.parser.Eval2;
import com.sdicons.repl.parser.EvalTrace;
import com.sdicons.repl.parser.Parser;
import junit.framework.Assert;

import org.junit.Test;


public class TestEvalTrace
{
    @Test
    public void testTrace()
    throws CommandException
    {
        Parser parser = new Parser();
        Eval2 eval = new Eval2();
        MathCmd.registerCommands(eval);
        eval.eval(parser.parseExpression("(defun fac (n) (if (> $n 0) (* $n (fac (- $n 1))) 1))"));
        Object lExpr =  parser.parseExpression("(fac 3)");

        EvalTrace trace = new EvalTrace(eval, lExpr);
        while (trace.hasMoreSteps())
        {
            System.out.println(trace.getStack());
            trace.step();
        }
        System.out.println(trace.getStack());

        Object lResult = trace.getResult();
        Assert.assertEquals(new BigDecimal("6"), lResult);
        //trace.terminate();
    }

}