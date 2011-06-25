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

import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

/**
 * It assumes a property file per library.
 * The property file should follow a specific structure:
 * - lib.<name>: Contain a short description of the library.
 * - <name>.toc: A list of all the commands in this libary.
 * - <name>.<command1>, <name>.<command2>, ... Each command should be prefixed with the library name.
 *
 * If no argumentis provided, the help will print a list of all libraries.
 * If help was requested about a library (using the library name), the help will print the description + the toc.
 * If help was requested about a command (using the command name), the command description will be printed.
 *
 */
public class HelpCmd
extends AbstractCommand
{
    private List<Properties> help = new LinkedList<Properties>();

    public HelpCmd(String ... aResources)
    {
        try
        {
            for(String aResource : aResources)
            {
                Properties lHelpfile = new Properties();
                lHelpfile.loadFromXML(this.getClass().getResourceAsStream(aResource));
                help.add(lHelpfile);
            }
        }
        catch (IOException e)
        {
        }
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        String lQuestion = null;
        if(aArgs.length > 1 && (aArgs[1] instanceof String)) lQuestion = (String) aArgs[1];

        // Get the printer from the context.
        final Object lWriterObj = aCtx.getBinding("*output");
        PrintWriter lWriter = null;
        if (lWriterObj instanceof PrintWriter) lWriter = (PrintWriter) lWriterObj;

        if(lQuestion == null)
        {
            // If no specific question was asked, we will print all
            // entries beginning with prefix "lib.".
            for(Properties lHelp : help)
            {
                for(Object lKey : lHelp.keySet())
                {
                    if(lKey instanceof String)
                    {
                        String lStrKey = (String) lKey;
                        if(lStrKey.startsWith("lib."))
                        {
                           if(lWriter != null) lWriter.print(lHelp.getProperty(lStrKey));
                        }
                    }
                }
            }
            return null;
        }
        else
        {
            // The user asked for a specif lib or for an individual command.
            // We will try to find a match for both of them.
            for(Properties lHelp : help)
            {
                for(Object lKey : lHelp.keySet())
                {
                    if(lKey instanceof String)
                    {
                        String lStrKey = (String) lKey;
                        if(lStrKey.equals("lib." + lQuestion))
                        {
                            // Found a match for a specific library.
                            // First we write the library header.
                            if(lWriter != null) lWriter.print(lHelp.getProperty(lStrKey));
                            // We also try to find a table of contents
                            // for that libary.
                            String lToc = lQuestion + ".toc";
                            if(lHelp.containsKey(lToc))
                                if(lWriter != null) lWriter.print(lHelp.getProperty(lToc));
                            return null;
                        }
                        else if(lStrKey.endsWith("." + lQuestion))
                        {
                            // Found a match for a specific command.
                            if(lWriter != null) lWriter.print(lHelp.getProperty(lStrKey));
                            return null;
                        }
                    }
                }
            }
            if(lWriter != null) lWriter.println("No help available on topic '" + lQuestion + "'.");
            return null;
        }
    }
}