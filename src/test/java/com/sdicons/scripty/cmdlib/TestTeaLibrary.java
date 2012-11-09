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

public class TestTeaLibrary
{
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException
    {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(TeaLibrary.class);
    }

    @Test
    public void encryptStandard()
    throws ProcessorException
    {
        Object lResult = scripty.process("tea-encrypt \"Bruno Ranschaert is my name\"");
        Assert.assertEquals("58eac04cbf4629f6b576c80570e0e1898e96a6105f0d3b2780b570873f21580e601f30ab6d365853601f30ab6d365853601f30ab6d365853", lResult);
    }

    @Test
    public void decryptStandard()
    throws ProcessorException
    {
        Object lResult = scripty.process("tea-decrypt 58eac04cbf4629f6b576c80570e0e1898e96a6105f0d3b2780b570873f21580e601f30ab6d365853601f30ab6d365853601f30ab6d365853");
        Assert.assertEquals("Bruno Ranschaert is my name", lResult);
    }

    @Test
    public void encryptPwd()
    throws ProcessorException
    {
        Object lResult = scripty.process("tea-encrypt \"Bruno Ranschaert is my name\" password=\"Lang leve de koning!\"");
        Assert.assertEquals("ebe7d6aff359a43aaf6bb157504eb980633005aa56fa84b5642b928a2d72d193d60c1af1435c043bd60c1af1435c043bd60c1af1435c043b", lResult);
    }

    @Test
    public void decryptPwd()
    throws ProcessorException
    {
        Object lResult = scripty.process("tea-decrypt ebe7d6aff359a43aaf6bb157504eb980633005aa56fa84b5642b928a2d72d193d60c1af1435c043bd60c1af1435c043bd60c1af1435c043b password=\"Lang leve de koning!\"");
        Assert.assertEquals("Bruno Ranschaert is my name", lResult);
    }
}
