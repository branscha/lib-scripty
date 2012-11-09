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
package com.sdicons.scripty.cmdlib;

import com.sdicons.scripty.ExtensionException;
import com.sdicons.scripty.ProcessorException;
import com.sdicons.scripty.ScriptyStreamProcessor;
import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

public class TestStringLibrary
{
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException
    {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(StringLibrary.class);
    }

    @Test
    public void isString1()
    throws ProcessorException
    {
        Object lResult = scripty.process("str? abc");
        Assert.assertTrue(lResult instanceof Boolean);
        Assert.assertTrue((Boolean) lResult);
    }

    @Test
    public void isString2()
    throws ProcessorException
    {
        Object lResult = scripty.process("str? '()");
        Assert.assertTrue(lResult instanceof Boolean);
        Assert.assertFalse((Boolean) lResult);
    }

    @Test
    public void isString3()
    throws ProcessorException
    {
        Object lResult = scripty.process("str? $null");
        Assert.assertTrue(lResult instanceof Boolean);
        Assert.assertFalse((Boolean) lResult);
    }

    @Test
    public void trim1()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-trim \"  abc  \"");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("abc", lResult);
    }

    @Test
    public void format1()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-format \"1-%s, 2-%s\" uno duo");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("1-uno, 2-duo", lResult);
    }

    @Test
    public void match1()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-match \"(a+)(b*)c\" aaabbbbbc");
        Assert.assertTrue(lResult instanceof List);
        List lListResult = (List) lResult;
        Assert.assertTrue(lListResult.size() ==3);
        Assert.assertEquals("aaabbbbbc", lListResult.get(0));
        Assert.assertEquals("aaa", lListResult.get(1));
        Assert.assertEquals("bbbbb", lListResult.get(2));
    }

    @Test
    public void match2()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-match* \"(\\d+)\\s*\" \"10 11 12 13\"");
        Assert.assertTrue(lResult instanceof List);
        List lListResult = (List) lResult;
        Assert.assertTrue(lListResult.size() == 4);

        Assert.assertEquals("10", ((List) lListResult.get(0)).get(1));
        Assert.assertEquals("11", ((List) lListResult.get(1)).get(1));
        Assert.assertEquals("12", ((List) lListResult.get(2)).get(1));
        Assert.assertEquals("13", ((List) lListResult.get(3)).get(1));
    }

    @Test
    public void isMatch1()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-match? \"(a+)(b*)c\" aaabbbbbc");
        Assert.assertTrue(lResult instanceof Boolean);
        Assert.assertTrue(((Boolean) lResult));
    }

    @Test
    public void isMatch2()
    throws ProcessorException
    {
        Object lResult = scripty.process("str-match? \"(a+)(b*)c\" aaabbbbb");
        Assert.assertTrue(lResult instanceof Boolean);
        Assert.assertFalse(((Boolean) lResult));
    }
}
