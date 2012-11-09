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
package com.sdicons.scripty.spec.args;

import com.sdicons.scripty.parser.IContext;
import com.sdicons.scripty.parser.Pair;
import com.sdicons.scripty.spec.type.ITypeSpec;
import com.sdicons.scripty.spec.type.TypeSpecException;

/**
 * Optional args are positional (have a fixed location). An optional parameter
 * can be omitted, in which case the default value will be used.
 * 
 * It is assumed that fixed args come first, then optional ones and 
 * eventually named parameters.
 * 
 * An optional parameter cannot be a pair, because it could conflict
 * with the named parameters which are grouped at the end of the list.
 *
 */
public class OptionalArg 
implements IArgSpec
{
    private ITypeSpec spec;
    private String specName;
    private Object defaultVal;
    
    public OptionalArg(ITypeSpec aSpec, Object aVal)
    {
        spec = aSpec;
        defaultVal = aVal;                
        specName = "opt: " + spec.getSpecName();
    }
    
    public String getSpecName()
    {     
        return specName;                    
    }

    public Object guard(Object[] aArgs, int aPos, IContext aCtx) 
    throws ArgSpecException
    {
        try
        {
            if(aPos < 0 || aPos >= aArgs.length) return spec.guard(defaultVal, aCtx);
            else if(aArgs[aPos] instanceof Pair) return spec.guard(defaultVal, aCtx);
            else return spec.guard(aArgs[aPos], aCtx);
        }
        catch (TypeSpecException e)
        {
            throw new ArgSpecException(String.format("Optional argument at position %d: %s", aPos, e.getMessage()));
        }
    }
}
