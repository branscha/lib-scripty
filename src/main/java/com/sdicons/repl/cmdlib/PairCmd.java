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

import com.sdicons.repl.parser.*;

/**
 * A Pair is an immutable parser artifact. There are only test functions and getter functions.
 * <ul>
 *    <li><b><code>pair?</code></b> Test whether an object is of type pair.<code>(pair? obj)</code></li>
 *    <li><b><code>pair-left</code></b> Get the left part of a pair.<code>(pair-left p)</code></li>
 *    <li><b><code>pair-right</code></b> Get the right part of a pair.<code>(pair-right p)</code></li>
 * </ul>
 *
 */
public class PairCmd
extends AbstractCommand
{
    public static enum PairCmdType {left, right, isPair};
    private PairCmdType type;

    public static void registerCommands(IRegistry aReg)
    {
        aReg.registerCommand("pair?", new PairCmd(PairCmdType.isPair));
        aReg.registerCommand("pair-left", new PairCmd(PairCmdType.left));
        aReg.registerCommand("pair-right", new PairCmd(PairCmdType.right));
    }

    public PairCmd(PairCmdType aType)
    {
        type = aType;
    }

    private Pair guardSinglePair(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2 || !(aArgs[1] instanceof Pair))
            throw new CommandException(String.format("ERROR: Command '%s' expects a single argument of type pair.", aArgs[0]));
        return (Pair) aArgs[1];
    }

    private Object guardSingleObject(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2)
            throw new CommandException(String.format("ERROR: Command '%s' expects a single argument.", aArgs[0]));
        return aArgs[1];
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        switch(type)
        {
            case left:
                return guardSinglePair(aArgs).getLeft();
            case right:
                return guardSinglePair(aArgs).getRight();
            case isPair:
                return (guardSingleObject(aArgs) instanceof Pair);
            default:
                throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[0]));
        }
    }
}
