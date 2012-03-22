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

package com.sdicons.scripty.cmdlib;

import com.sdicons.scripty.ExtensionException;
import com.sdicons.scripty.ProcessorException;
import com.sdicons.scripty.ScriptyStreamProcessor;
import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

public class TestListLibrary
{
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException
    {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(ListLibrary.class);
    }

    @Test
    public void createList1()
    throws ProcessorException
    {
        Object lResult = scripty.process("list");
        Assert.assertTrue(lResult instanceof java.util.List);
        Assert.assertTrue(((List) lResult).isEmpty());
    }

    @Test
    public void createList2()
    throws ProcessorException
    {
        Object lResult = scripty.process("list 1 2 3 4 5");
        Assert.assertTrue(lResult instanceof java.util.List);
        Assert.assertTrue(((List) lResult).size() == 5);
    }

    @Test
    public void isEmpty1()
    throws ProcessorException
    {
        Object lResult = scripty.process("empty? (list)");
        Assert.assertTrue(Boolean.TRUE.equals(lResult));
    }

    @Test
    public void isEmpty2()
    throws ProcessorException
    {
        Object lResult = scripty.process("empty? (list 1 2 3 4 5)");
        Assert.assertTrue(Boolean.FALSE.equals(lResult));
    }

    @Test(expected = ProcessorException.class)
    public void isEmpty3()
    throws ProcessorException
    {
        scripty.process("empty?");
        Assert.fail();
    }

    @Test(expected = ProcessorException.class)
    public void isList1()
    throws ProcessorException
    {
        scripty.process("list?");
        Assert.fail();
    }

    @Test
    public void isList2()
    throws ProcessorException
    {
        Object lResult = scripty.process("list? (list 1 2 3 4 5)");
        Assert.assertTrue(Boolean.TRUE.equals(lResult));
    }

    @Test
    public void isList3()
    throws ProcessorException
    {
        Object lResult = scripty.process("list? (list)");
        Assert.assertTrue(Boolean.TRUE.equals(lResult));
    }

    @Test
    public void isList4()
    throws ProcessorException
    {
        Object lResult = scripty.process("list? abc");
        Assert.assertTrue(Boolean.FALSE.equals(lResult));
    }

    @Test
    public void isMember1()
    throws ProcessorException
    {
        Object lResult = scripty.process("member? (list 1 2 3 4 5) 13");
        Assert.assertTrue(Boolean.FALSE.equals(lResult));
    }

    @Test
    public void isMember2()
    throws ProcessorException
    {
        Object lResult = scripty.process("member? (list 1 2 3 4 5) 3");
        Assert.assertTrue(Boolean.TRUE.equals(lResult));
    }

    @Test
    public void car1()
    throws ProcessorException
    {
        Object lResult = scripty.process("car (list 17 13 19 23)");
        Assert.assertTrue("17".equals(lResult));
    }

    @Test(expected = ProcessorException.class)
    public void car2()
    throws ProcessorException
    {
        scripty.process("car (list)");
        Assert.fail();
    }

    @Test(expected = ProcessorException.class)
    public void car3()
    throws ProcessorException
    {
        scripty.process("car ()");
        Assert.fail();
    }

    @Test
    public void car4()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                "   (let " +
                "      (lst=(list 17 13 19 23)) " +
                "      (progn (car $lst) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 4);
    }

    @Test
    public void cdr1()
    throws ProcessorException
    {
        Object lResult = scripty.process("cdr (list 17 13 19 23)");
        Assert.assertTrue(lResult instanceof  List);
        List lList = (List) lResult;
        Assert.assertTrue(lList.size() == 3);
        Assert.assertTrue("13".equals(lList.get(0)));
        Assert.assertTrue("19".equals(lList.get(1)));
        Assert.assertTrue("23".equals(lList.get(2)));
    }

    @Test(expected = ProcessorException.class)
    public void cdr2()
    throws ProcessorException
    {
        scripty.process("cdr ()");
        Assert.fail();
    }

    @Test
    public void shift1()
    throws ProcessorException
    {
        Object lResult = scripty.process("shift (list 17 13 19 23)");
        Assert.assertTrue("17".equals(lResult));
    }

    @Test(expected = ProcessorException.class)
    public void shift2()
    throws ProcessorException
    {
        scripty.process("shift (list)");
        Assert.fail();
    }

    @Test(expected = ProcessorException.class)
    public void shift3()
    throws ProcessorException
    {
        scripty.process("shift ()");
        Assert.fail();
    }

    @Test
    public void shift4()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (shift $lst) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 3);
    }

    @Test
    public void unshift1()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (unshift $lst 2 3 5) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 7);
        Assert.assertTrue("5".equals(((List) lResult).get(0)));
        Assert.assertTrue("3".equals(((List) lResult).get(1)));
        Assert.assertTrue("2".equals(((List) lResult).get(2)));
    }

    @Test
    public void cons1()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (cons 5 $lst) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 5);
        Assert.assertTrue("5".equals(((List) lResult).get(0)));
    }

    @Test
    public void pop1()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (pop $lst) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 3);
        Assert.assertTrue("17".equals(((List) lResult).get(0)));
        Assert.assertTrue("13".equals(((List) lResult).get(1)));
        Assert.assertTrue("19".equals(((List) lResult).get(2)));
    }

    @Test
    public void pop2()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (pop $lst))");
        Assert.assertTrue("23".equals(lResult));
    }

    @Test
    public void push1()
    throws ProcessorException
    {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (push $lst 2 3 5) $lst))");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertTrue(((List) lResult).size() == 7);
        Assert.assertTrue("5".equals(((List) lResult).get(6)));
        Assert.assertTrue("3".equals(((List) lResult).get(5)));
        Assert.assertTrue("2".equals(((List) lResult).get(4)));
    }

    // append

}
