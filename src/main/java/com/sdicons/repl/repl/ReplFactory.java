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

package com.sdicons.repl.repl;

import java.io.InputStream;
import java.io.OutputStream;

import com.sdicons.repl.cmdlib.ExitCmd;
import com.sdicons.repl.cmdlib.PrintCmd;
import com.sdicons.repl.cmdlib.UuidCmd;

public class ReplFactory implements IReplFactory
{
    public IRepl createRepl(InputStream aIn, OutputStream aOut)
    {
        IRepl lRepl =  new Repl(aIn, aOut, aOut);
        lRepl.registerCommand("echo", new PrintCmd());
        lRepl.registerCommand("uuid", new UuidCmd());
        lRepl.registerCommand("exit", new ExitCmd());  
        return lRepl;
    }
}
