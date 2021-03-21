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
import branscha.scripty.parser.Pair;
import org.junit.Before;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertFalse;

public class TestPairLibrary {
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(PairLibrary.class);
    }

    @Test
    public void testPairCreation()
    throws ProcessorException {
        Object lResult = scripty.process("let (p1=a=uno p2=b=duo p3=c=ter) $p1");
        assertTrue(lResult instanceof Pair);
        Pair lPair = (Pair) lResult;
        assertEquals("a", lPair.getLeft());
        assertEquals("uno", lPair.getRight());
    }

    @Test
    public void isPair1()
    throws ProcessorException {
        Object lResult = scripty.process("let (p1=a=uno p2=b=duo p3=c=ter) (pair? $p1)");
        assertTrue(lResult instanceof Boolean);
        assertTrue((Boolean) lResult);
    }

    @Test
    public void isPair2()
    throws ProcessorException {
        Object lResult = scripty.process("pair? abc");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void isPair3()
    throws ProcessorException {
        Object lResult = scripty.process("pair? $null");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void pairLeft1()
    throws ProcessorException {
        Object lResult = scripty.process("let (p1=a=uno p2=b=duo p3=c=ter) (pair-left $p1)");
        assertTrue(lResult instanceof String);
        assertEquals("a", lResult);
    }

    @Test
    public void pairRight1()
    throws ProcessorException {
        Object lResult = scripty.process("let (p1=a=uno p2=b=duo p3=c=ter) (pair-right $p1)");
        assertTrue(lResult instanceof String);
        assertEquals("uno", lResult);
    }
}
