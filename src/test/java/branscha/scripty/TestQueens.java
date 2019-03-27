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
package branscha.scripty;

import branscha.scripty.annot.ScriptyCommand;
import branscha.scripty.annot.ScriptyLibrary;
import branscha.scripty.annot.ScriptyLibraryType;
import branscha.scripty.cmdlib.ListLibrary;
import branscha.scripty.cmdlib.LoadLibrary;
import branscha.scripty.cmdlib.MathLibrary;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestQueens
{
    private ScriptyStreamProcessor scripty;
    private PrintPlusCmd printer;

    @ScriptyLibrary(type= ScriptyLibraryType.INSTANCE)
    public static class PrintPlusCmd
    {
        private int counter = 0;

        @ScriptyCommand
        public void print()
        {
            counter ++;
        }

        @ScriptyCommand
        public void println()
        {
            counter++;
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
    throws ExtensionException, ProcessorException
    {
        scripty = new ScriptyStreamProcessor();
        printer = new PrintPlusCmd();

        scripty.addLibraryClasses(MathLibrary.class, LoadLibrary.class, ListLibrary.class);
        scripty.addLibraryInstances(printer);
        scripty.process("(load cp:/queens.lsp)");
    }

    public int queens(int n)
    throws ProcessorException
    {
        printer.reset();
        scripty.process(String.format("(queens %d)", n));
        return printer.getCounter();
    }

    @Test
    public void queens()
    throws ProcessorException
    {
        Assert.assertEquals(1, queens(1));
        Assert.assertEquals(0, queens(2));
        Assert.assertEquals(0, queens(3));
        Assert.assertEquals(2, queens(4));
        Assert.assertEquals(10, queens(5));
        Assert.assertEquals(4, queens(6));
        Assert.assertEquals(40, queens(7));
        //Assert.assertEquals(92, queens(8));
        //Assert.assertEquals(352, queens(9));
        //Assert.assertEquals(724, queens(10));
    }
}
