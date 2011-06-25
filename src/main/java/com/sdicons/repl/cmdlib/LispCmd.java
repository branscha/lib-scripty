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
import com.sdicons.repl.spec.args.*;
import com.sdicons.repl.spec.type.CheckedListType;
import com.sdicons.repl.spec.type.ITypeSpec;
import com.sdicons.repl.spec.type.InstanceType;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Some Lisp like commands that act on lists. This module provides the basic
 * data manipulation commands, they are not built-in but provided as a separate module.
 * The semantics deviates from common lisp because we based our lists on Java lists and not on cons constructs.
 * Consing for example modifies the existing list, while in Lisp it creates a new version, the original binding refers to the sublist.
 * This can lead to surprising results if one is used to the original Lisp semantics.
 * <p>
 * <ul>
 *    <li><b>list?</b> A test to see if the argument is a list or not. The other command types are only applicable if this test turns out positive.</li>
 *    <li><b>empty?</b> A test to see if the list is empty.</li>
 *    <li><b>member?</b> A test to see if an element is part of the list or not. <code>(member? &lt;list> &lt;el>)</code></li>
 *    <li><b>car</b> The first element of the list.</li>
 *    <li><b>cdr</b> A copy of the list without the first element. Non destructible on the original.</li>
 *    <li><b>pop</b> Get the element at the end of the list. Modifies the list and returns the element.</li>
 *    <li><b>shift</b> Get the first element of the list and modifies the list. It returns the element.</li>
 *    <li><b>push</b> Add one or more elements at the end of the list. Modifies the list and returns the list. <code>(push &lt;list> el1 ... eln)</code></li> *
 *    <li><b>unshift</b> Insert one or more elements at the beginning of the list. <code>(unshift &lt;list> el1 ... eln)</code></li>
 *    <li><b>cons</b> Insert one element at the beginning of the list. The list is modified, destructible on the original.<code>(cons el &lt;list>)</code></li>
 *    <li><b>append</b> Append two or more lists into a single new list. Non destructible.</li>
 *    <li><b>size</b> The number of elements in the list. The result is a string representing an integer.</li>
 *    <li><b>dup</b> Make a shallow copy of the list.</li>
 *    <li><b>null?</b> A test to see if the argument is null.</li>
 * </ul>
 *
 */
public class LispCmd
extends AbstractCommand
{
    public static enum LispCmdType {list, isEmpty, isList, isMember, car, cdr, size, shift, pop, push, unshift, append, dup, isNull}

    /** Register all the commands in the module to a registry.
     *
     * This method registers all commands in the module to a registry.
     * You can also do this manually if you want to use other command names or if you want
     * to be more selective about the commands that you want to make available in the repl.
     *
     * @param aReg A command registry.
     */
    public static void registerCommands(IRegistry aReg)
    {
        aReg.registerCommand("list", new LispCmd(LispCmdType.list));
        aReg.registerCommand("empty?", new LispCmd(LispCmdType.isEmpty));
        aReg.registerCommand("list?", new LispCmd(LispCmdType.isList));
        aReg.registerCommand("member?", new LispCmd(LispCmdType.isMember));
        aReg.registerCommand("car", new LispCmd(LispCmdType.car));
        aReg.registerCommand("cdr", new LispCmd(LispCmdType.cdr));
        aReg.registerCommand("shift", new LispCmd(LispCmdType.shift));
        aReg.registerCommand("unshift", new LispCmd(LispCmdType.unshift));
        aReg.registerMacro("cons", new AbstractMacro()
        {
            public Object transform(List anExpr)
            throws CommandException
            {
                if(anExpr.size() != 3) throw new CommandException("ERROR: 'cons' expects 2 arguments, an element and a list.");
                final List<Object> lMacro = new ArrayList<Object>();
                lMacro.add("unshift");
                lMacro.add(anExpr.get(2));
                lMacro.add(anExpr.get(1));
                return lMacro;
            }
        });
        aReg.registerCommand("pop", new LispCmd(LispCmdType.pop));
        aReg.registerCommand("push", new LispCmd(LispCmdType.push));
        aReg.registerCommand("append", new LispCmd(LispCmdType.append));
        aReg.registerCommand("size", new LispCmd(LispCmdType.size));
        aReg.registerCommand("dup", new LispCmd(LispCmdType.dup));
        aReg.registerCommand("null?", new LispCmd(LispCmdType.isNull));
    }

    // TYPES
    ////////
    private static final ITypeSpec TLIST = new InstanceType(List.class, "list", false);
    private static final ITypeSpec TOBJ = new InstanceType(Object.class, "whatever", true);
    private static final ITypeSpec TNELIST = new CheckedListType(TOBJ, 1, -1);
    // ARGS
    ////////
    private static final FixedArg FLIST = new FixedArg(TLIST);
    private static final FixedArg FNELIST = new FixedArg(TNELIST);
    private static final FixedArg FOBJ = new FixedArg(TOBJ);
    private static final VarArg VEXPR = new VarArg(TOBJ);
    private static final VarArg VLIST = new VarArg(TLIST);
    // PARAMETER LISTS
    //////////////////
    private static final IArgList FLIST_ = new StdArgList(new FixedArg[]{FLIST}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FNELIST_ = new StdArgList(new FixedArg[]{FNELIST}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FOBJ_ = new StdArgList(new FixedArg[]{FOBJ}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FLIST_FOBJ_ = new StdArgList(new FixedArg[]{FLIST, FOBJ}, new OptionalArg[]{}, new NamedArg[]{});
    private static final IArgList FLIST_VOBJ_ = new VarArgList(new FixedArg[]{FLIST}, VEXPR, -1, -1, new NamedArg[]{});
    private static final IArgList VLIST_ = new VarArgList(new FixedArg[]{}, VLIST, -1, -1, new NamedArg[]{});

    private static final int IDX_CMD = 0;
    private static final int IDX_LST = 1;
    private static final int IDX_OBJ = 2;

    private LispCmdType type;

    public LispCmd(LispCmdType aType)
    {
        type = aType;
    }

    @SuppressWarnings("unchecked")
    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        try
        {
            switch(type)
            {
                case isList:
                {
                    final Object[] lArgs = FOBJ_.guard(aArgs, aCtx);
                    return (lArgs[1] instanceof List);
                }
                case isNull:
                {
                    final Object[] lArgs = FOBJ_.guard(aArgs, aCtx);
                    return (lArgs[1] == null);
                }
                case car:
                {
                    final Object[] lArgs = FNELIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    return lList.get(0);
                }
                case cdr:
                {
                    final Object[] lArgs = FLIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    // Take a copy of the list.
                    List<Object> lResult = new LinkedList<Object>(lList);
                    if(lResult.size() > 0) lResult.remove(0);
                    return lResult;
                }
                case isEmpty:
                {
                    final Object[] lArgs = FLIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    return lList.size() <= 0;
                }
                case pop:
                {
                    final Object[] lArgs = FNELIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    return lList.remove(lList.size() - 1);
                }
                case shift:
                {
                    final Object[] lArgs = FNELIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    return lList.remove(0);
                }
                case size:
                {
                    final Object[] lArgs = FLIST_.guard(aArgs, aCtx);
                    List<Object> lList = (List<Object>) lArgs[IDX_LST];
                    // Note that we return a string an d not an integer.
                    // We use strings as our basic communication medium.
                    return Integer.toString(lList.size());
                }
                case isMember:
                {
                    final Object[] lArgs = FLIST_FOBJ_.guard(aArgs, aCtx);
                    final List lList = (List) lArgs[IDX_LST];
                    final Object lElement = lArgs[IDX_OBJ];
                    return lList.contains(lElement);
                }
                case push:
                {
                    final Object[] lArgs = FLIST_VOBJ_.guard(aArgs, aCtx);
                    final List lList = (List) lArgs[IDX_LST];
                    for(int i = 2; i < lArgs.length; i++) lList.add(aArgs[i]);
                    return lList;
                }
                case unshift:
                {
                    final Object[] lArgs = FLIST_VOBJ_.guard(aArgs, aCtx);
                    final List lList = (List) lArgs[IDX_LST];
                    for(int i = 2; i < lArgs.length; i++) lList.add(0, aArgs[i]);
                    return lList;
                }
                case append:
                {
                    final Object[] lArgs = VLIST_.guard(aArgs, aCtx);
                    // Initialize the result.
                    final List<Object> lResult = new LinkedList<Object>();
                    for (int i = 1; i < lArgs.length; i++)
                    {
                        List lPart = (List) lArgs[i];
                        lResult.addAll(lPart);
                    }
                    return lResult;
                }
                case list:
                {
                    List lResult = new LinkedList();
                    for(int i = 1; i < aArgs.length; i++) lResult.add(aArgs[i]);
                    return lResult;
                }
                case dup:
                {
                    final Object[] lArgs = FLIST_.guard(aArgs, aCtx);
                    final List<Object> lResult = new LinkedList<Object>();
                    lResult.addAll((List)lArgs[1]);
                    return lResult;
                }
                default:
                    throw new CommandException(String.format("ERROR: Command '%s' internal error." , aArgs[IDX_CMD]));
            }
        }
        catch (ArgSpecException e)
        {
            throw new CommandException(String.format("ERROR: Command '%s' argument error.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
    }
}