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

package com.sdicons.repl.parser;

import java.io.File;
import java.io.FileInputStream;

public class Interpreter
{
    public static final void main(String[] aArgs)
    {
        // Argument check.
        if(aArgs.length < 1) throw new IllegalArgumentException("Expected a script name as a parameter.");
        final String lFilename = aArgs[0];
        final File lFile = new File(lFilename);
        if(!lFile.exists()) throw new IllegalArgumentException(String.format("The file '%s' does not exist.", lFilename));
        if(!lFile.canRead()) throw new IllegalArgumentException(String.format("The file '%s' is not readable.", lFilename));
        
        // Keep track of our position.
        int lLine = 1;
        int lCol = 1;
                
        // Execute the script.
        try
        {
            // Initialize the interpreter components.
            final StreamBuffer lBuf = new StreamBuffer(new FileInputStream(lFile));
            final Parser lParser = new Parser();
            final com.sdicons.repl.parser.IEval lEval = new Eval();
            
            lEval.registerCommand("print", new AbstractCommand() 
            {
                public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
                throws CommandException
                {
                   if(aArgs.length > 0) System.out.println(aArgs[1].toString());
                   return null;
                }            
            });
            
            while(!lBuf.eof())
            { 
                // Keep track of the location of the
                // next expression.
                lLine = lBuf.getLineNr();
                lCol = lBuf.getColNr();
                
                final Object lExpr = lParser.parseExpression(lBuf);
                if(lExpr instanceof Token)
                {
                    Token lToken = (Token) lExpr;
                    final String lMsg = String.format("ERROR: While parsing '%s'.\n%s", lFile.getAbsolutePath(), lToken.getValue());
                    System.out.println(lMsg);
                    break;
                }
                lEval.eval(lExpr);
            }
        }
        catch (Exception e)
        {
            final String lMsg = String.format("*** Fatal error encoutered on line %d, col %d.", lLine, lCol);
            System.out.println(lMsg);
            e.printStackTrace();
        }       
    }
}
