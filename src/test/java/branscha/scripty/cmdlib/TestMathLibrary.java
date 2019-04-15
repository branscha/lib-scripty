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
package branscha.scripty.cmdlib;

import branscha.scripty.ExtensionException;
import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyStreamProcessor;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static junit.framework.TestCase.*;
import static org.junit.Assert.assertFalse;

public class TestMathLibrary {
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(MathLibrary.class);
    }

    @Test
    public void addNoArgs()
    throws ProcessorException {
        Object lResult = scripty.process("+");
        assertEquals(BigDecimal.ZERO, lResult);
    }

    @Test
    public void addOneArg()
    throws ProcessorException {
        Object lResult = scripty.process("+ 13");
        assertEquals(BigDecimal.valueOf(13), lResult);
    }

    @Test
    public void addMultiple()
    throws ProcessorException {
        Object lResult = scripty.process("+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25");
        assertEquals(BigDecimal.valueOf(325), lResult);
    }

    @Test(expected = ProcessorException.class)
    public void minusNoArg()
    throws ProcessorException {
        // It should throw an error, more than one
        // argument is needed ...
        scripty.process("-");
        fail();
    }

    @Test
    public void minusMultiple()
    throws ProcessorException {
        Object lResult = scripty.process("- 13 17 20");
        assertEquals(BigDecimal.valueOf(-24), lResult);
    }

    @Test
    public void div()
    throws ProcessorException {
        Object lResult = scripty.process("/ 111 3");
        assertEquals(BigDecimal.valueOf(37), lResult);
    }

    @Test
    public void mult()
    throws ProcessorException {
        Object lResult = scripty.process("* 1 2 3 4 5 6");
        assertEquals(BigDecimal.valueOf(720), lResult);
    }

    @Test
    public void pow1()
    throws ProcessorException {
        Object lResult = scripty.process("^ 2 8");
        assertEquals(BigDecimal.valueOf(256), lResult);
    }

    @Test(expected = ProcessorException.class)
    public void pow2()
    throws ProcessorException {
        // This should generate an exception because the second
        // argument should be an integer.
        scripty.process("^ 2 8.3");
        fail();
    }

    @Test
    public void rem()
    throws ProcessorException {
        Object lResult = scripty.process("rem 123 7");
        assertEquals(BigDecimal.valueOf(4), lResult);
    }

    @Test
    public void abs1()
    throws ProcessorException {
        Object lResult = scripty.process("abs -123");
        assertEquals(BigDecimal.valueOf(123), lResult);
    }

    @Test
    public void abs2()
    throws ProcessorException {
        Object lResult = scripty.process("abs 456");
        assertEquals(BigDecimal.valueOf(456), lResult);
    }

    @Test
    public void test1()
    throws ProcessorException {
        Object lResult = scripty.process("number? 123");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void test2()
    throws ProcessorException {
        Object lResult = scripty.process("number? abc");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void lt1()
    throws ProcessorException {
        Object lResult = scripty.process("< 13 14");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void lt2()
    throws ProcessorException {
        Object lResult = scripty.process("< 13 13");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void lt3()
    throws ProcessorException {
        Object lResult = scripty.process("< 14 13");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void gt1()
    throws ProcessorException {
        Object lResult = scripty.process("> 23 22");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void gt2()
    throws ProcessorException {
        Object lResult = scripty.process("> 22 22");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void gt3()
    throws ProcessorException {
        Object lResult = scripty.process("> 22 23");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void le1()
    throws ProcessorException {
        Object lResult = scripty.process("<= 13 14");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void le2()
    throws ProcessorException {
        Object lResult = scripty.process("<= 13 13");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void le3()
    throws ProcessorException {
        Object lResult = scripty.process("<= 14 13");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void ge1()
    throws ProcessorException {
        Object lResult = scripty.process(">= 23 22");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void ge2()
    throws ProcessorException {
        Object lResult = scripty.process(">= 22 22");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void ge3()
    throws ProcessorException {
        Object lResult = scripty.process(">= 22 23");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void eq1()
    throws ProcessorException {
        Object lResult = scripty.process("= 13 13");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void eq2()
    throws ProcessorException {
        Object lResult = scripty.process("= 14 13");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void zero1()
    throws ProcessorException {
        Object lResult = scripty.process("zero? 0");
        assertTrue((Boolean) lResult);
    }


    @Test
    public void zero2()
    throws ProcessorException {
        Object lResult = scripty.process("zero? 0.001");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void convert()
    throws ProcessorException {
        Object lResult = scripty.process("float->int 10.3333557");
        assertEquals(BigInteger.valueOf(10), lResult);
    }

    @Test
    public void fin1()
    throws ProcessorException {
        Object lResult = scripty.process("fin 10.3333557");
        assertEquals(BigDecimal.valueOf(10.33), lResult);
    }

    @Test
    public void fin2()
    throws ProcessorException {
        Object lResult = scripty.process("fin 10.337");
        assertEquals(BigDecimal.valueOf(10.34), lResult);
    }
}
