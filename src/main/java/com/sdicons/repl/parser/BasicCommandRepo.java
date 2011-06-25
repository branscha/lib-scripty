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

import java.util.HashMap;
import java.util.Map;

public class BasicCommandRepo
implements ICommandRepo
{
	protected Map<String, ICommand> commands = new HashMap<String, ICommand>();

	public ICommand getCommand(String aName)
	{
		return commands.get(aName);
	}

	public boolean hasCommand(String aName)
	{
		return commands.containsKey(aName);
	}

	public void registerCommand(String aName, ICommand aCommand)
	{
		commands.put(aName, aCommand);
	}

    public Map<String, ICommand> dumpCommands()
    {
        return  new HashMap<String, ICommand>(commands);
    }
}
