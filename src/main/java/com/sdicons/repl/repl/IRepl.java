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

import com.sdicons.repl.parser.CommandException;
import com.sdicons.repl.parser.IContext;
import com.sdicons.repl.parser.IRegistry;

public interface IRepl
extends IRegistry
{
    // Change the prompt.
    public String getPrompt();
    public void setPrompt(String aPrompt);

    // Starting and stopping the repl.
    public void start();
    public void stop();

    // Access the context.
    public IContext getContext();
    void setContext(IContext context);

    // Execute a command. The expression language is not specified here, it can be
    // whatever the implementation offers.
    public Object exec(String anExpression)
    throws CommandException;
}
