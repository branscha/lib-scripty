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
package com.sdicons.scripty.parser;

import java.util.HashMap;
import java.util.Map;

public class CompositeContext
implements IContext
{
	IContext main, backing;

	public CompositeContext(IContext aChildCtx, IContext aParentCtx)
	{
		main = aChildCtx;
		backing = aParentCtx;
	}

	public Object getBinding(String aKey)
	{
		if(main.isBound(aKey)) return main.getBinding(aKey);
		else return backing.getBinding(aKey);
	}

    public void removeBinding(String aKey)
    {
        if(main.isBound(aKey)) main.removeBinding(aKey);
        else backing.removeBinding(aKey);
    }

    public void setBinding(String aKey, Object aValue)
    throws CommandException
    {
        if(main.isBound(aKey)) main.setBinding(aKey, aValue);
        else if (backing.isBound(aKey)) backing.setBinding(aKey, aValue);
        else throw new CommandException(String.format("There is no binding for '%s' in the context.", aKey==null?"null":aKey));
    }

	public boolean isBound(String aKey)
	{
		return main.isBound(aKey) || backing.isBound(aKey);
	}

    public IContext getRootContext()
    {
        if(backing != null) return backing.getRootContext();
        else if(main != null) return main.getRootContext();
        else return null;
    }

    public void defBinding(String aKey, Object aValue)
    {
        main.defBinding(aKey, aValue);
    }

    public Map<String, Object> dumpBindings()
    {
        Map<String, Object> lBackingDump = backing.dumpBindings();
        Map<String, Object> lMainDump = main.dumpBindings();
        Map<String, Object> lDump = new HashMap<String, Object>(lBackingDump);
        lDump.putAll(lMainDump);
        return lDump;
    }

    @Override
    public String toString()
    {
        final StringBuilder lBuilder = new StringBuilder();
        lBuilder.append(main.toString());
        lBuilder.append("----------\n");
        lBuilder.append(backing.toString());
        return lBuilder.toString();
    }
}
