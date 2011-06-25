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
import com.sdicons.repl.spec.type.ITypeSpec;
import com.sdicons.repl.spec.type.TypeSpecException;

/**
 * Fixed arguments are positional (have a fixed location) and type.
 * They are required, the user has to provide them.
 */
public class FixedArg 
implements IArgSpec
{
    private ITypeSpec spec;
    private String specName;
    
    public FixedArg(ITypeSpec aSpec)
    {
        spec = aSpec;            
        specName = "fixed: " + spec.getSpecName();
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
            if(aPos < 0 || aPos >= aArgs.length) 
                throw new ArgSpecException(String.format("The required argument at position %d, type %s missing.", aPos, spec.getSpecName()));
            else return spec.guard(aArgs[aPos], aCtx);
        }
        catch (TypeSpecException e)
        {
            throw new ArgSpecException(String.format("The required argument at position %d: %s", aPos, e.getMessage()));
        }
    }
}
