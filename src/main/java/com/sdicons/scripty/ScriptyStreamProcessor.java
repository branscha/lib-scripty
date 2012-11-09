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

import com.sdicons.scripty.repl.ReplEngineException;

import java.io.InputStream;
import java.io.Reader;

public class ScriptyStreamProcessor
extends ScriptyCapable
{
    public ScriptyStreamProcessor()
    {
        // Initialize with a new ReplEngine.
    }
    
    public ScriptyStreamProcessor(ScriptyCapable aScriptyFacade)
    {
        // Initialize with an existing ReplEngine.
        super.setReplEngine(aScriptyFacade.getReplEngine());
    }
    
    public Object process(String aExpression)
    throws ProcessorException
    {
        try
        {
            return getReplEngine().startNonInteractive(aExpression);
        }
        catch (ReplEngineException e)
        {
            throw new ProcessorException(e.getMessage(), e);
        }
    }
    
    public Object process(InputStream aStream)
    throws ProcessorException
    {
        try
        {
            return getReplEngine().startNonInteractive(aStream);
        } 
        catch (ReplEngineException e)
        {
            throw new ProcessorException(e.getMessage(), e);
        }
    }
    
    public Object process(Reader aReader)
    throws ProcessorException
    {
        try
        {
            return getReplEngine().startNonInteractive(aReader);
        } 
        catch (ReplEngineException e)
        {
            throw new ProcessorException(e.getMessage(), e);
        }
    }
}
