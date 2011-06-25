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
import com.sdicons.repl.parser.*;
import com.sdicons.repl.repl.Repl;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * User: Bruno Ranschaert
 * Date: Jul 17, 2010
 * Time: 12:29:05 AM
 */
public class TestQueens
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

    public int queens(int n)
    throws CommandException
    {
        repl.exec("(load cp:/queens.lsp)");
        printer.reset();
        repl.exec("(queens " + n + ")");
        return printer.getCounter();
    }

    @Test
    public void queens()
    throws CommandException
    {
        Assert.assertEquals(1, queens(1));
        Assert.assertEquals(0, queens(2));
        Assert.assertEquals(0, queens(3));
        Assert.assertEquals(2, queens(4));
        Assert.assertEquals(10, queens(5));
        Assert.assertEquals(4, queens(6));
        Assert.assertEquals(40, queens(7));
        Assert.assertEquals(92, queens(8));
        Assert.assertEquals(352, queens(9));
        //Assert.assertEquals(724, queens(10));
    }
}
