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

/**
 * There are 3 types of parameters in a standard argument list.
 * 1. Fixed and required arguments. Each with its own type.
 * 2. Optional. Each argument has its own type. These cannot be of type Pair, because this might conflict with the named arguments.
 *    The optional parameters can have a default value which will be used when the argument is not present.
 * 3. Named (optional or required). These are pairs at the end of the command line.
 *    The named parameters can have a default value.
 */
public class StdArgList 
implements IArgList
{
    public static final IArgList NOARG = new StdArgList(new FixedArg[]{}, new OptionalArg[]{}, new NamedArg[]{});
    
    private FixedArg req[];
    private OptionalArg[] opt;
    private NamedArg[] named;
    
    public StdArgList(FixedArg aReq[], OptionalArg[] aOpt, NamedArg[] aNamed)
    {
        req = aReq;
        opt = aOpt;
        named = aNamed;
    }
    
    /**
     * The ars are expected to have the form ("CMD", arg-1, arg-2, ... arg-n).
     */
    public Object[] guard(Object[] aArgs, IContext aCtx) 
    throws ArgSpecException
    {
        // Create a new argument list where we will accumulate the
        // converted results.
        Object[] lNewArgs = new Object[1 + req.length + opt.length + named.length];
        // Copy the command name to the new argument list, the structure will remain the same.
        lNewArgs[0] = aArgs[0];
        
        // Check the fixed.
        if(aArgs.length - 1 < req.length)
            throw new ArgSpecException(String.format("Too few arguments. Expected at least %d arguments but received %d.", req.length, aArgs.length - 1));
        
        // We skip the command name.
        int lArgIdx = 1;
        for(int i = 0; i < req.length; i++) 
        {
            lNewArgs[lArgIdx] = req[i].guard(aArgs, lArgIdx++, aCtx);
        }
        
        // Check the optional ones.
        // Start looking after the fixed args, 
        // Provide the expected index.
        for(IArgSpec lSpec: opt) 
        {
            lNewArgs[lArgIdx] = lSpec.guard(aArgs, lArgIdx++, aCtx);
        }
        
        // If there are still arguments left that are not pairs there are 
        // too many arguments.
        if(lArgIdx < aArgs.length && !(aArgs[lArgIdx] instanceof Pair))
            throw new ArgSpecException(String.format("Too many arguments. Expected at most %d arguments.", req.length + opt.length));
        
        // Check the named args.
        // We look for all pairs at the end of the argument list. We will only
        // consider these trailing pairs.
        int lStartNamed = aArgs.length - 1;
        while(lStartNamed > 0 && (aArgs[lStartNamed] instanceof Pair)) lStartNamed --;
        // Now we can resolve the named arguments within this range.
        for(IArgSpec lSpec: named) 
        {
            lNewArgs[lArgIdx++] = lSpec.guard(aArgs, lStartNamed, aCtx);
        }        
        // Finally we go looking for spurious named parameters that were not specified ...
        for(int i = lStartNamed; i < aArgs.length; i++)
        {
            if(aArgs[i] instanceof Pair)
            {
                final Pair lPair = (Pair) aArgs[i];
                if(!(lPair.getLeft() instanceof String))
                    throw new ArgSpecException(String.format("Found an badly formed named argument, where the name is not a string but an instance of type '%s'.", lPair.getLeft()==null?"null":lPair.getLeft().getClass().getCanonicalName()));
                String lPairName = (String) lPair.getLeft();
                boolean found = false;
                for(int j = 0; j < named.length; j++)
                {
                    if(named[j].getName().equals(lPairName))
                    {
                        found = true;
                        break;                        
                    }
                }
                if(!found)
                    throw new ArgSpecException(String.format("Found an unexpected named argument '%s'.", lPairName));                
            }
        }
        return lNewArgs;
    }
}
