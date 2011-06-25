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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * String commands that can be optionally included in the REPL.
 * It is an optional command module, it is not included by default.
 */
public class StrCmd
extends AbstractCommand
{
    public enum StrCmdType {strTrim, isStr, strFormat, strMatch, isStrMatch, strMatchRepeat};

    private StrCmdType type = StrCmdType.strTrim;

    public StrCmd(StrCmdType aType)
    {
        type = aType;
    }

    public static void registerCommands(IRegistry aReg)
    {
        // Check if an arbitrary object is a string.
        aReg.registerCommand("str?", new StrCmd(StrCmdType.isStr));
        aReg.registerCommand("str-trim", new StrCmd(StrCmdType.strTrim));
        aReg.registerCommand("str-format", new StrCmd(StrCmdType.strFormat));
        // Do a single match, the result is a list of matched groups.
        // Even is there is no group in the pattern, the global match is
        // always available.
        aReg.registerCommand("str-match", new StrCmd(StrCmdType.strMatch));
        // Repeatedly match the pattern. The result is a list of lists of matches.
        // It is the same as str-match but it is applied repeatedly to the string.
        aReg.registerCommand("str-match*", new StrCmd(StrCmdType.strMatchRepeat));
        // Check if a string complies to a pattern. The result is a boolean.
        aReg.registerCommand("str-match?", new StrCmd(StrCmdType.isStrMatch));
    }

    private Object guardSingleArg(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2 )
            throw new CommandException(String.format("ERROR: Command '%s' expects a single argument.", aArgs[0]));
        return aArgs[1];
    }

    private String guardSingleString(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 2 || !(aArgs[1] instanceof String))
            throw new CommandException(String.format("ERROR: Command '%s' expects a single string argument.", aArgs[0]));
        return (String) aArgs[1];
    }

    private String guardTwoStrings(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length != 3 || !(aArgs[1] instanceof String) || !(aArgs[2] instanceof String))
            throw new CommandException(String.format("ERROR: Command '%s' expects two strings, a regexp and string to match.", aArgs[0]));
        return (String) aArgs[1];
    }

    private String guardStringAndObjects(Object[]aArgs)
    throws CommandException
    {
        if(aArgs.length < 2 || !(aArgs[1] instanceof String))
            throw new CommandException(String.format("ERROR: Command '%s' expects a format string and an optional number of arguments.", aArgs[0]));
        return (String) aArgs[1];
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        switch(type)
        {
            case strMatchRepeat:
            {
                guardTwoStrings(aArgs);
                final Pattern lRegexp = Pattern.compile((String) aArgs[1]);
                final Matcher lMatcher = lRegexp.matcher((String) aArgs[2]);
                final List<List<String>> lRes = new ArrayList<List<String>>();
                while(lMatcher.find())
                {
                    final List<String> lMatch = new ArrayList<String>();
                    // Note: there is one more group then the count indicates.
                    // This is because of group 0 which matches the complete string.
                    int lCount = lMatcher.groupCount();
                    for(int i = 0; i <= lCount; i++) lMatch.add(lMatcher.group(i));
                    lRes.add(lMatch);
                }
                return lRes;
            }
            case isStrMatch:
            {
                guardTwoStrings(aArgs);
                final Pattern lRegexp = Pattern.compile((String) aArgs[1]);
                final Matcher lMatcher = lRegexp.matcher((String) aArgs[2]);
                return lMatcher.matches();
            }
            case strMatch:
            {
                guardTwoStrings(aArgs);
                final Pattern lRegexp = Pattern.compile((String) aArgs[1]);
                final Matcher lMatcher = lRegexp.matcher((String) aArgs[2]);
                final List<String> lRes = new ArrayList<String>();
                if(lMatcher.find())
                {
                    // Note: there is one more group then the count indicates.
                    // This is because of group 0 which matches the complete string.
                    int lCount = lMatcher.groupCount();
                    for(int i = 0; i <= lCount; i++) lRes.add(lMatcher.group(i));
                }
                return lRes;
            }
            case isStr:
                return guardSingleArg(aArgs) instanceof String;
            case strTrim:
                return guardSingleString(aArgs).trim();
            case strFormat:
            {
                String lFormat = guardStringAndObjects(aArgs);
                Object[] lObjArgs = new Object[aArgs.length - 2];
                for(int i = 0; i < lObjArgs.length; i++) lObjArgs[i] = aArgs[i + 2];
                return String.format(lFormat, lObjArgs);
            }
            default:
                throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[0]));
        }
    }
}
