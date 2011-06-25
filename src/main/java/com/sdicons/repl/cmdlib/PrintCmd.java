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

import com.sdicons.repl.parser.AbstractCommand;
import com.sdicons.repl.parser.CommandException;
import com.sdicons.repl.parser.IContext;
import com.sdicons.repl.parser.IEval;

import java.io.PrintWriter;

public class PrintCmd
extends AbstractCommand
{
    public PrintCmd()
    {
    }

    public Object execute(IEval aEval, IContext aContext, Object[] aArgs)
    throws CommandException
    {
        // Get the printer from the context.
        final Object lWriterObj = aContext.getBinding("*output");
        PrintWriter lWriter = null;
        if (lWriterObj instanceof PrintWriter) lWriter = (PrintWriter) lWriterObj;

        final StringBuilder lBuilder = new StringBuilder();
        for (int i = 1; i < aArgs.length; i++)
        {
            final Object lObj = aArgs[i];
            if (lObj != null) lBuilder.append(lObj.toString()).append(" ");
        }
        final String lResult = lBuilder.toString();
        if (lWriter != null) lWriter.println(lResult);
        return lResult;
    }
}
