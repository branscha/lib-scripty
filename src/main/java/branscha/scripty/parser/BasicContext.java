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
package branscha.scripty.parser;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class BasicContext
implements IContext
{
	private Map<String, Object> context = new HashMap<String, Object>();

	public Object getBinding(String aKey)
	{
		return context.get(aKey);
	}

	public void setBinding(String aKey, Object aValue)
	throws CommandException
	{
	    if(isBound(aKey)) context.put(aKey, aValue);
	    else throw new CommandException(String.format("There is no binding for '%s' in the context.", aKey==null?"null":aKey));
	}

	public boolean isBound(String aKey)
	{
		return context.containsKey(aKey);
	}

    public void removeBinding(String aKey)
    {
        context.remove(aKey);
    }

    public IContext getRootContext()
    {
        return this;
    }

    public void defBinding(String aKey, Object aValue)
    {
        context.put(aKey, aValue);
    }

    public Map<String, Object> dumpBindings()
    {
        // Create a new map.
        return new HashMap<String, Object>(context);
    }

    @Override
    public String toString()
    {
        final StringBuilder lBuilder = new StringBuilder();
        Map<String, Object> lDump = dumpBindings();
        Set<String> lKeyset = new TreeSet<String>(lDump.keySet());

        for(String lKey : lKeyset)
        {
            Object lVal = lDump.get(lKey);
            lBuilder.append(lKey);
            lBuilder.append(" = ");
            lBuilder.append(lVal == null?"null":lVal.toString());
            lBuilder.append("\n");
        }
        return lBuilder.toString();
    }
}
