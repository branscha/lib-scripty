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

package com.sdicons.repl.cmdlib;

import java.util.List;

import com.sdicons.repl.parser.Pair;

public class CmdUtil
{    
    /**
     * Concatenate error messages from nested exceptions to get an error message that contains
     * as much information as possible.
     * 
     * @param e
     * @return
     */
    public static String concatExceptionMessages(Throwable e)
    {
        final StringBuilder lBuilder = new StringBuilder(e.getMessage());
        Throwable t = e;
        while (t.getCause() != null)
        {
            t = t.getCause();
            lBuilder.append("\n").append(e.getCause().getMessage());
        }
        return lBuilder.toString();
    }
    
    /**
     * Find a specific option on the command line. We start from the last option
     * to the first, so that the latter takes precedence on the former.
     * 
     * @param aOptionName The string name of the option.
     * @param aArgs The command argument list.
     * @return null if the option is not found, the option otherwise.
     */
    public static Object findOption(String aOptionName, Object[] aArgs)
    {
        for(int i = aArgs.length - 1; i >= 0; i--)
        {
            if(aArgs[i] != null && aArgs[i] instanceof Pair)
            {
                Pair lPair = (Pair) aArgs[i];
                Object lLeft = lPair.getLeft();
                Object lRight = lPair.getRight();
                if(lLeft != null && lLeft instanceof String && aOptionName.equals(lLeft)) 
                    return lRight;
            }
        }
        return null;        
    }
    
    public static Object findOption(String aOptionName, List<?> aArgs)
    {
        for(int i = aArgs.size() - 1; i >= 0; i--)
        {
            if(aArgs.get(i) != null && aArgs.get(i) instanceof Pair)
            {
                Pair lPair = (Pair) aArgs.get(i);
                Object lLeft = lPair.getLeft();
                Object lRight = lPair.getRight();
                if(lLeft != null && lLeft instanceof String && aOptionName.equals(lLeft)) 
                    return lRight;
            }
        }
        return null;        
    }
}
