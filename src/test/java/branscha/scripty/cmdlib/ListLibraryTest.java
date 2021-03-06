/* ******************************************************************************
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
package branscha.scripty.cmdlib;

import branscha.scripty.ExtensionException;
import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyStreamProcessor;
import junit.framework.Assert;
import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.TestCase.*;
import static org.junit.Assert.assertFalse;

public class ListLibraryTest {

    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(ListLibrary.class);
    }

    @Test
    // 'list' without arguments should create an empty list.
    public void createList1()
    throws ProcessorException {
        Object lResult = scripty.process("list");
        assertTrue(lResult instanceof java.util.List);
        assertTrue(((List) lResult).isEmpty());
    }

    @Test
    // Standard case to create a list.
    public void createList2()
    throws ProcessorException {
        Object lResult = scripty.process("list 1 2 3 4 5");
        assertTrue(lResult instanceof java.util.List);
        assertEquals(5, ((List) lResult).size());
    }

    @Test
    // An empty list should test as empty.
    public void isEmpty1()
    throws ProcessorException {
        Object lResult = scripty.process("empty? (list)");
        assertEquals(Boolean.TRUE, lResult);
    }

    @Test
    // A non-empty list should not be empty.
    public void isEmpty2()
    throws ProcessorException {
        Object lResult = scripty.process("empty? (list 1 2 3 4 5)");
        assertEquals(Boolean.FALSE, lResult);
    }

    @Test(expected = ProcessorException.class)
    // 'empty?' without arguments should throw an exception.
    public void isEmpty3()
    throws ProcessorException {
        scripty.process("empty?");
        fail();
    }

    @Test(expected = ProcessorException.class)
    // 'list?' without argument should throw an exception.
    public void isList1()
    throws ProcessorException {
        scripty.process("list?");
        fail();
    }

    @Test
    // Normal case, test that a list is a list.
    public void isList2()
    throws ProcessorException {
        Object lResult = scripty.process("list? (list 1 2 3 4 5)");
        assertEquals(Boolean.TRUE, lResult);
    }

    @Test
    // An empty list is a list as well.
    public void isList3()
    throws ProcessorException {
        Object lResult = scripty.process("list? (list)");
        assertEquals(Boolean.TRUE, lResult);
    }

    @Test
    // A string is not a list.
    public void isList4()
    throws ProcessorException {
        Object lResult = scripty.process("list? abc");
        assertEquals(Boolean.FALSE, lResult);
    }

    @Test
    // The 'list?' should not blow up on null values.
    public void isList5()
    throws ProcessorException {
        Object lResult = scripty.process("list? $null");
        assertEquals(Boolean.FALSE, lResult);
    }

    @Test
    public void isMember1()
    throws ProcessorException {
        Object lResult = scripty.process("member? (list 1 2 3 4 5) 13");
        assertEquals(Boolean.FALSE, lResult);
    }

    @Test
    public void isMember2()
    throws ProcessorException {
        Object lResult = scripty.process("member? (list 1 2 3 4 5) 3");
        assertEquals(Boolean.TRUE, lResult);
    }

    @Test
    public void car1()
    throws ProcessorException {
        Object lResult = scripty.process("car (list 17 13 19 23)");
        assertEquals("17", lResult);
    }

    @Test(expected = ProcessorException.class)
    public void car2()
    throws ProcessorException {
        scripty.process("car (list)");
        fail();
    }

    @Test(expected = ProcessorException.class)
    public void car3()
    throws ProcessorException {
        scripty.process("car ()");
        fail();
    }

    @Test
    public void car4()
    throws ProcessorException {
        Object lResult = scripty.process(
                "   (let " +
                        "      (lst=(list 17 13 19 23)) " +
                        "      (progn (car $lst) $lst))");
        assertTrue(lResult instanceof List);
        assertTrue(((List) lResult).size() == 4);
    }

    @Test
    public void cdr1()
    throws ProcessorException {
        Object lResult = scripty.process("cdr (list 17 13 19 23)");
        assertTrue(lResult instanceof List);
        List lList = (List) lResult;
        assertEquals(3, lList.size());
        assertEquals("13", lList.get(0));
        assertEquals("19", lList.get(1));
        assertEquals("23", lList.get(2));
    }

    @Test(expected = ProcessorException.class)
    public void cdr2()
    throws ProcessorException {
        scripty.process("cdr ()");
        fail();
    }

    @Test
    public void shift1()
    throws ProcessorException {
        Object lResult = scripty.process("shift (list 17 13 19 23)");
        assertEquals("17", lResult);
    }

    @Test(expected = ProcessorException.class)
    public void shift2()
    throws ProcessorException {
        scripty.process("shift (list)");
        fail();
    }

    @Test(expected = ProcessorException.class)
    public void shift3()
    throws ProcessorException {
        scripty.process("shift ()");
        fail();
    }

    @Test
    public void shift4()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (shift $lst) $lst))");
        assertTrue(lResult instanceof List);
        assertEquals(3, ((List) lResult).size());
    }

    @Test
    public void unshift1()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (unshift $lst 2 3 5) $lst))");
        assertTrue(lResult instanceof List);
        assertEquals(7, ((List) lResult).size());
        assertEquals("5", ((List) lResult).get(0));
        assertEquals("3", ((List) lResult).get(1));
        assertEquals("2", ((List) lResult).get(2));
    }

    @Test
    // We test the side effect of cons, viz. it changes the original list.
    public void cons1()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (cons 5 $lst) $lst))");
        assertTrue(lResult instanceof List);
        assertEquals(5, ((List) lResult).size());
        assertEquals("5", ((List) lResult).get(0));
    }

    @Test
    // We test the side effect of pop, viz. it changes
    // the original list.
    public void pop1()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (pop $lst) $lst))");
        assertTrue(lResult instanceof List);
        assertEquals(3, ((List) lResult).size());
        assertEquals("17", ((List) lResult).get(0));
        assertEquals("13", ((List) lResult).get(1));
        assertEquals("19", ((List) lResult).get(2));
    }

    @Test
    // We test the main functionality of pop, viz. it returns
    // the last element.
    public void pop2()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (pop $lst))");
        assertEquals("23", lResult);
    }

    @Test
    // We test for the side efffect of 'push' by pushing on the list, then returning the original list.
    public void push1()
    throws ProcessorException {
        Object lResult = scripty.process(
                " (let " +
                        " (lst=(list 17 13 19 23)) " +
                        " (progn (push $lst 2 3 5) $lst))");
        assertTrue(lResult instanceof List);
        assertEquals(7, ((List) lResult).size());
        assertEquals("5", ((List) lResult).get(6));
        assertEquals("3", ((List) lResult).get(5));
        assertEquals("2", ((List) lResult).get(4));
    }

    @Test
    // We test the main functionality of the append.
    public void append1()
    throws ProcessorException {
        Object lResult = scripty.process(
                "(let " +
                        " (lst1=(list 1 2 3) lst2=(list 4 5 6) lst3=(list 7 8 9)) " +
                        " (append $lst1 $lst2 $lst3)" +
                        ")");
        assertTrue(lResult instanceof List);
        List lList = (List) lResult;
        assertEquals(9, lList.size());
        for (int i = 1; i < 10; i++) assertTrue(lList.contains("" + i));
    }

    @Test
    // The original lists should be unmodified.
    // Here we check out lst1 and see if it remained unchanged.
    public void append2()
    throws ProcessorException {
        Object lResult = scripty.process(
                "(let " +
                        " (lst1=(list 1 2 3) lst2=(list 4 5 6) lst3=(list 7 8 9)) " +
                        " (progn (append $lst1 $lst2 $lst3) $lst1)" +
                        ")");
        assertTrue(lResult instanceof List);
        List lList = (List) lResult;
        assertEquals(3, lList.size());
        for (int i = 1; i < 4; i++) assertTrue(lList.contains("" + i));
    }

    @Test
    // Main functionality of 'size' on a normal list.
    public void size1()
    throws ProcessorException {
        Object lResult = scripty.process(
                "(let " +
                        " (lst1=(list 1 2 3 4 5 6 7 8 9)) " +
                        " (size $lst1)" +
                        ")");
        assertEquals("9", lResult);
    }

    @Test
    // The empty list should have size 0.
    public void size2()
    throws ProcessorException {
        Object lResult = scripty.process(
                "(let " +
                        " (lst1='()) " +
                        " (size $lst1)" +
                        ")");
        assertEquals("0", lResult);
    }

    @Test
    // Test the main dup functionality.
    public void dup()
    throws ProcessorException {
        // Prepare a list with 1000 elements.
        final List lOrig = new ArrayList();
        for (int i = 0; i < 1000; i++) lOrig.add(i);

        // Duplicate it.
        scripty.getContext().defBinding("lst", lOrig);
        Object lResult = scripty.process("dup $lst");

        // Test that it is equals but not the same.
        assertEquals(lOrig, lResult);
        assertNotSame(lOrig, lResult);
    }

    @Test
    // 'null?' should not blow up on null values.
    //
    public void isNull()
    throws ProcessorException {
        // Duplicate it.
        scripty.getContext().defBinding("one", null);
        scripty.getContext().defBinding("two", "abc");

        assertTrue((Boolean) scripty.process("null? $one"));
        assertFalse((Boolean) scripty.process("null? $two"));
    }
}
