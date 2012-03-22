/*
 * Scripty Programming Language
 * Copyright (C) 2010-2012 Bruno Ranschaert, S.D.I.-Consulting BVBA
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

package com.sdicons.scripty.spec.args;

import com.sdicons.scripty.ExtensionException;
import com.sdicons.scripty.ProcessorException;
import com.sdicons.scripty.ScriptyStreamProcessor;
import com.sdicons.scripty.annot.ScriptyArg;
import com.sdicons.scripty.annot.ScriptyCommand;
import com.sdicons.scripty.annot.ScriptyParam;
import com.sdicons.scripty.annot.ScriptyStdArgList;
import com.sdicons.scripty.cmdlib.TeaLibrary;
import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestArgListBuilderUtil
{
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
            throws ExtensionException
    {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(TeaLibrary.class);
    }

    public static class AnyTypeLibrary
    {
        @ScriptyCommand(name="go")
        @ScriptyStdArgList(fixed={@ScriptyArg(name="arg", type="Any")})
        public static Object command(@ScriptyParam("arg") Object aArg)
        {
            return aArg;
        }

        @ScriptyCommand(name="go2")
        @ScriptyStdArgList(fixed={@ScriptyArg(name="arg", type="Any nullAllowed=true")})
        public static Object command2(@ScriptyParam("arg") Object aArg)
        {
            return aArg;
        }
        
    }
    
    @Test
    public void testAny1()
    throws ExtensionException, ProcessorException
    {
        scripty.addLibraryClasses(AnyTypeLibrary.class);
        Object lResult = scripty.process("go teststring");
        Assert.assertTrue("teststring".equals(lResult));
    }

    @Test(expected = ProcessorException.class)
    // The 'Any' type does not allow null values if not
    // explicitly allowed.
    public void testAny2()
    throws ExtensionException, ProcessorException
    {
        scripty.addLibraryClasses(AnyTypeLibrary.class);
        scripty.process("go $null");
        Assert.fail();
    }

    @Test
    // The 'Any' type should allow null values if nullAllowed=true is
    // specified.
    public void testAny3()
    throws ExtensionException, ProcessorException
    {
        scripty.addLibraryClasses(AnyTypeLibrary.class);
        Object lResult = scripty.process("go2 $null");
        Assert.assertTrue(null == lResult);
    }
}
