/*
 * Scripty Programming Language
 * Copyright (C) 2010-2011 Bruno Ranschaert, S.D.I.-Consulting BVBA
 * http://www.sdi-consulting.be
 * mailto://info@sdi-consulting.be
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package com.sdicons.repl.spec.args;

import com.sdicons.repl.parser.IContext;
import com.sdicons.repl.parser.Pair;
import com.sdicons.repl.spec.type.ITypeSpec;
import com.sdicons.repl.spec.type.TypeSpecException;

public class NamedArg 
implements IArgSpec
{
    private String paramName;
    private Object value;
    private ITypeSpec valueSpec;
    private boolean optional = true;
    private String specName;
    
    public NamedArg(String aName, ITypeSpec aValSpec, Object aValue, boolean aOptional)
    {
        paramName = aName;
        value = aValue;
        valueSpec = aValSpec;        
        optional = aOptional;
        
        specName = paramName + "=" + valueSpec.getSpecName();
    }
    
    public String getSpecName()
    {        
        return specName;
    }

    public Object guard(Object[] aArgs, int aPos, IContext aCtx) 
    throws ArgSpecException
    {
        for(int i = aPos; i < aArgs.length; i++)
        {
            if(aArgs[i] instanceof Pair)
            {
                Pair lPair = (Pair) aArgs[i];
                if(paramName.equals(lPair.getLeft()))
                {                    
                    try
                    {
                        Pair lNewPair =  new Pair(paramName, valueSpec.guard(lPair.getRight(), aCtx));
                        aArgs[i] = lNewPair;
                        return lNewPair.getRight();
                    }
                    catch (TypeSpecException e)
                    {
                        throw new ArgSpecException(String.format("Named argument '%s': %s", paramName, e.getMessage()));
                    }
                }
            }
        }        
        if(optional) return value;
        else throw new ArgSpecException(String.format("Missing named argument '%s'.", paramName));
    }
    
    public String getName()
    {
        return paramName;
    }
}
