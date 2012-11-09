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
package com.sdicons.scripty;

import com.sdicons.scripty.annot.ScriptyCommand;
import com.sdicons.scripty.annot.ScriptyLibrary;
import com.sdicons.scripty.annot.ScriptyLibraryType;
import com.sdicons.scripty.cmdlib.ListLibrary;
import com.sdicons.scripty.cmdlib.LoadLibrary;
import com.sdicons.scripty.cmdlib.MathLibrary;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestHanoi
{
    private PrintPlusCmd printer;
    private ScriptyStreamProcessor scripty;

    @ScriptyLibrary(type = ScriptyLibraryType.INSTANCE)
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

        scripty.addLibraryClasses(LoadLibrary.class, MathLibrary.class, ListLibrary.class);
        scripty.addLibraryInstances(printer);

        scripty.process("(load cp:/hanoi.lsp)");
    }

    public int hanoi(int n)
    throws  ProcessorException
    {
        printer.reset();
        scripty.process(String.format("(hanoi %d)", n));
        return printer.getCounter();
    }

    @Test
    public void hanoi()
    throws ProcessorException
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
