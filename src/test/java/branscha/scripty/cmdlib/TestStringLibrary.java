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
import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

public class TestStringLibrary {
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(StringLibrary.class);
    }

    @Test
    public void isString1()
    throws ProcessorException {
        Object lResult = scripty.process("str? abc");
        assertTrue(lResult instanceof Boolean);
        assertTrue((Boolean) lResult);
    }

    @Test
    public void isString2()
    throws ProcessorException {
        Object lResult = scripty.process("str? '()");
        assertTrue(lResult instanceof Boolean);
        assertFalse((Boolean) lResult);
    }

    @Test
    public void isString3()
    throws ProcessorException {
        Object lResult = scripty.process("str? $null");
        assertTrue(lResult instanceof Boolean);
        assertFalse((Boolean) lResult);
    }

    @Test
    public void trim1()
    throws ProcessorException {
        Object lResult = scripty.process("str-trim \"  abc  \"");
        assertTrue(lResult instanceof String);
        assertEquals("abc", lResult);
    }

    @Test
    public void format1()
    throws ProcessorException {
        Object lResult = scripty.process("str-format \"1-%s, 2-%s\" uno duo");
        assertTrue(lResult instanceof String);
        assertEquals("1-uno, 2-duo", lResult);
    }

    @Test
    public void match1()
    throws ProcessorException {
        Object lResult = scripty.process("str-match \"(a+)(b*)c\" aaabbbbbc");
        assertTrue(lResult instanceof List);
        List lListResult = (List) lResult;
        TestCase.assertEquals(3, lListResult.size());
        assertEquals("aaabbbbbc", lListResult.get(0));
        assertEquals("aaa", lListResult.get(1));
        assertEquals("bbbbb", lListResult.get(2));
    }

    @Test
    public void match2()
    throws ProcessorException {
        Object lResult = scripty.process("str-match* \"(\\d+)\\s*\" \"10 11 12 13\"");
        assertTrue(lResult instanceof List);
        List lListResult = (List) lResult;
        TestCase.assertEquals(4, lListResult.size());

        assertEquals("10", ((List) lListResult.get(0)).get(1));
        assertEquals("11", ((List) lListResult.get(1)).get(1));
        assertEquals("12", ((List) lListResult.get(2)).get(1));
        assertEquals("13", ((List) lListResult.get(3)).get(1));
    }

    @Test
    public void isMatch1()
    throws ProcessorException {
        Object lResult = scripty.process("str-match? \"(a+)(b*)c\" aaabbbbbc");
        assertTrue(lResult instanceof Boolean);
        assertTrue(((Boolean) lResult));
    }

    @Test
    public void isMatch2()
    throws ProcessorException {
        Object lResult = scripty.process("str-match? \"(a+)(b*)c\" aaabbbbb");
        assertTrue(lResult instanceof Boolean);
        assertFalse(((Boolean) lResult));
    }
}
