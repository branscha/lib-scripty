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

package com.sdicons.repl;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.sdicons.repl.parser.Pair;
import com.sdicons.repl.spec.args.ArgSpecException;
import com.sdicons.repl.spec.args.IArgSpec;
import com.sdicons.repl.spec.args.NamedArg;
import com.sdicons.repl.spec.type.CheckedListType;
import com.sdicons.repl.spec.type.ITypeSpec;
import com.sdicons.repl.spec.type.InstanceType;
import com.sdicons.repl.spec.type.IntegerType;
import com.sdicons.repl.spec.type.TypeSpecException;

public class TestSpecs
{
    @Test
    public void testClassSpec()
    throws TypeSpecException
    {
        InstanceType lSpec = new InstanceType(Integer.class, false);
        lSpec.guard(10, null);        
    }
    
    @Test
    public void testIntList()
    throws TypeSpecException
    {
        ITypeSpec lSpec = new CheckedListType(new IntegerType());
        List lList = new ArrayList();
        lList.add(13);
        lList.add(17);
        lList.add(Long.valueOf(5000000000L));
        Object[] args = new Object[] {lList};
        lSpec.guard(lList, null);
    }
    
    @Test
    public void testNamedArgs()
    throws ArgSpecException
    {
        IArgSpec lSpec = new NamedArg("oele", new IntegerType(), Integer.valueOf(13), true);
        Object[] args = new Object [] {1, 2, 3, new Pair("oele", Integer.valueOf(17))};
        lSpec.guard(args, 0, null);
        System.out.println(args);
    }
}
