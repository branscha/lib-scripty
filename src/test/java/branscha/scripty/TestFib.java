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

import branscha.scripty.cmdlib.ListLibrary;
import branscha.scripty.cmdlib.LoadLibrary;
import branscha.scripty.cmdlib.MathLibrary;
import branscha.scripty.spec.type.BigDecimalType;
import branscha.scripty.spec.type.ITypeSpec;
import branscha.scripty.spec.type.TypeSpecException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;

public class TestFib {
    private ScriptyStreamProcessor scripty;

    @Before
    public void setup()
    throws ExtensionException, ProcessorException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(LoadLibrary.class, MathLibrary.class, ListLibrary.class);
        scripty.process("(load cp:/fib.lsp)");
    }

    private ITypeSpec bdspec = new BigDecimalType();

    public BigDecimal fib(int n)
    throws ProcessorException, TypeSpecException {
        Object lResult = scripty.process(String.format("(fib %d)", n));
        return (BigDecimal) bdspec.guard(lResult, scripty.getContext());
    }

    public BigDecimal fib2(int n)
    throws ProcessorException, TypeSpecException {
        Object lResult = scripty.process(String.format("(fib2 %d)", n));
        return (BigDecimal) bdspec.guard(lResult, scripty.getContext());
    }

    @Test
    public void fib()
    throws ProcessorException, TypeSpecException {
        for (int i = 2; i < 20; i++) {
            Assert.assertEquals(fib(i).floatValue(), fib2(i).floatValue(), 0.0001);
        }
    }
}
