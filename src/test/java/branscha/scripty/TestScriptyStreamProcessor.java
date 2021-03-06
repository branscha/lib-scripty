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
package branscha.scripty;

import branscha.scripty.testlib.MyCommandLib;
import branscha.scripty.testlib.MyCommandLib2;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class TestScriptyStreamProcessor {

    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize() {
        scripty = new ScriptyStreamProcessor();
    }

    @Test
    public void testMyCommandLib()
    throws ExtensionException, ProcessorException {
        scripty.addLibraryClasses(MyCommandLib.class);
        Object result = scripty.process("add 1 2");
        assertNotNull(result);
        assertTrue(result instanceof Integer);
        assertEquals("expression result", 3, result);

        result = scripty.process("inverse 11 12 add");
        assertNotNull(result);

        scripty.process("def-bruno abc");
        Object val = scripty.getContext().getBinding("bruno");
        assertEquals("abc", val);
    }

    @Test
    public void testMyCommandLib2()
    throws ExtensionException {
        scripty.addLibraryClasses(MyCommandLib2.class);
    }
}
