/*******************************************************************************
 * The MIT License
 * Copyright (c) 2012 Bruno Ranschaert
 * lib-scripty
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
package com.sdicons.scripty.spec;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.sdicons.scripty.parser.Pair;
import com.sdicons.scripty.spec.args.ArgSpecException;
import com.sdicons.scripty.spec.args.IArgSpec;
import com.sdicons.scripty.spec.args.NamedArg;
import com.sdicons.scripty.spec.type.CheckedListType;
import com.sdicons.scripty.spec.type.ITypeSpec;
import com.sdicons.scripty.spec.type.InstanceType;
import com.sdicons.scripty.spec.type.IntegerType;
import com.sdicons.scripty.spec.type.TypeSpecException;

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
