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

import com.sdicons.repl.cmdlib.LispCmd;
import com.sdicons.repl.cmdlib.LoadCmd;
import com.sdicons.repl.cmdlib.MathCmd;
import com.sdicons.repl.cmdlib.PrintCmd;
import com.sdicons.repl.parser.CommandException;
import com.sdicons.repl.parser.IContext;
import com.sdicons.repl.parser.IEval;
import com.sdicons.repl.repl.Repl;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * User: Bruno Ranschaert
 * Date: Jul 17, 2010
 * Time: 4:20:05 PM
 */
public class TestHanoi
{
    private Repl repl;
    private PrintPlusCmd printer;

    private static class PrintPlusCmd extends PrintCmd
    {
        private int counter = 0;

        public Object execute(IEval aEval, IContext aContext, Object[] aArgs) throws CommandException
        {
            counter ++;
            return super.execute(aEval, aContext, aArgs);
        }

        public int getCounter()
        {
            return counter;
        }

        public void reset()
        {
            counter = 0;
        }
    }

    @Before
    public void setup()
    {
        repl = new Repl(System.in, System.out, System.out);
        printer = new PrintPlusCmd();

        MathCmd.registerCommands(repl);
        LoadCmd.registerCommands(repl);
        LispCmd.registerCommands(repl);
        repl.registerCommand("print", printer);
    }

    public int hanoi(int n)
    throws CommandException
    {
        repl.exec("(load cp:/hanoi.lsp)");
        printer.reset();
        repl.exec("(hanoi " + n + ")");
        return printer.getCounter();
    }

    @Test
    public void hanoi()
    throws CommandException
    {
        Assert.assertEquals(1, hanoi(1));
        Assert.assertEquals(3, hanoi(2));
        Assert.assertEquals(7, hanoi(3));
        Assert.assertEquals(15, hanoi(4));
        Assert.assertEquals(31, hanoi(5));
        Assert.assertEquals(63, hanoi(6));
        Assert.assertEquals(127, hanoi(7));
        Assert.assertEquals(255, hanoi(8));
    }
}
