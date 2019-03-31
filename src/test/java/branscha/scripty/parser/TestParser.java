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
package branscha.scripty.parser;

import java.io.InputStream;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestParser {
    private Parser parser;

    @Before
    public void setup() {
        parser = new Parser();
    }

    @Test
    public void atoms() {
        Object lResult = parser.parseExpression("oele");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("oele", lResult);

        lResult = parser.parseExpression("0");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("0", lResult);

        lResult = parser.parseExpression("12345");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("12345", lResult);

        lResult = parser.parseExpression("\"abc\"");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("abc", lResult);

        lResult = parser.parseExpression("\"\\n\"");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("\n", lResult);

        lResult = parser.parseExpression("\"\\t\"");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("\t", lResult);

        lResult = parser.parseExpression("\"\\\\\"");
        Assert.assertTrue(lResult instanceof String);
        Assert.assertEquals("\\", lResult);
    }

    @Test
    public void i18n() {
        Object lResult = parser.parseExpression("(� � � � �)");
        Assert.assertTrue(lResult instanceof List);
        Assert.assertEquals(((List) lResult).size(), 5);
    }

    @Test
    public void simpleList() {
        // Empty list.
        Object lResult = parser.parseExpression("()");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertEquals(lList.size(), 0);

        // Non-empty standard list.
        lResult = parser.parseExpression("(abc 123 oele boele makkis voele)");
        Assert.assertTrue(lResult instanceof List);
        lList = (List<Object>) lResult;
        Assert.assertTrue(lList.size() == 6);
        Assert.assertEquals(lList.get(0), "abc");
        Assert.assertEquals(lList.get(1), "123");
        Assert.assertEquals(lList.get(2), "oele");
        Assert.assertEquals(lList.get(3), "boele");
        Assert.assertEquals(lList.get(4), "makkis");
        Assert.assertEquals(lList.get(5), "voele");
    }

    @Test
    public void simpleList2() {
        // Check space handling, all whitespace outside of strings should
        // be skipped. Check the handling of tabs.
        Object lResult = parser.parseExpression("(        abc 123 oele \tboele   makkis voele                )");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertTrue(lList.size() == 6);
        Assert.assertEquals(lList.get(0), "abc");
        Assert.assertEquals(lList.get(1), "123");
        Assert.assertEquals(lList.get(2), "oele");
        Assert.assertEquals(lList.get(3), "boele");
        Assert.assertEquals(lList.get(4), "makkis");
        Assert.assertEquals(lList.get(5), "voele");
    }

    @Test
    public void simpleList3() {
        // Test simple delimited strings.
        Object lResult = parser.parseExpression("(abc 123 \"oele boele makkis\" voele)");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertTrue(lList.size() == 4);
        Assert.assertEquals(lList.get(0), "abc");
        Assert.assertEquals(lList.get(1), "123");
        Assert.assertEquals(lList.get(2), "oele boele makkis");
        Assert.assertEquals(lList.get(3), "voele");
    }

    @Test
    public void nestedList() {
        // Standard nesting.
        Object lResult = parser.parseExpression("(abc () (123 oele boele (makkis) voele))");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertTrue(lList.size() == 3);

        // Exreme nesting
        lResult = parser.parseExpression("((((((((((x))))))))))");
        Assert.assertTrue(lResult instanceof List);
        lList = (List<Object>) lResult;
        for (int i = 0; i < 9; i++) {
            Assert.assertTrue(lList.get(0) instanceof List);
            lList = (List<Object>) lList.get(0);
        }
        Assert.assertEquals(lList.get(0), "x");
    }

    @Test
    public void quotedList() {
        Object lResult = parser.parseExpression("'(a b c d e)");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertEquals(lList.get(0), "quote");
        Assert.assertTrue(lList.get(1) instanceof List);
    }

    @Test
    public void pairing() {
        Object lResult = parser.parseExpression("(key1=val1 key2=() key3=(a b c))");
        Assert.assertTrue(lResult instanceof List);
        List<Object> lList = (List<Object>) lResult;
        Assert.assertTrue(lList.size() == 3);
        Assert.assertTrue(lList.get(0) instanceof Pair);
        Assert.assertTrue(lList.get(1) instanceof Pair);
        Assert.assertTrue(lList.get(2) instanceof Pair);
    }

    @Test
    public void openList() {
        // Counter example, should produce an error.
        Object lResult = parser.parseExpression("(abc 123 oele boele makkis voele");
        Assert.assertTrue(lResult instanceof Token);
        Token lToken = (Token) lResult;
        Assert.assertTrue(lToken.isErroneous());
    }

    @Test
    public void openList2() {
        // Counter example, shoule produce an error.

        Object lResult = parser.parseExpression("(abc 123 oele ( boele makkis voele");
        Assert.assertTrue(lResult instanceof Token);
        Token lToken = (Token) lResult;
        Assert.assertTrue(lToken.isErroneous());
    }

    @Test
    public void parseFile() {
        InputStream lIn = TestParser.class.getResourceAsStream("/branscha/repl/cmds.lsp");
        StreamBuffer lBuf = new StreamBuffer(lIn);
        while (!lBuf.eof()) {
            Object lResult = parser.parseExpression(lBuf);
            Assert.assertTrue(lResult != null);
        }
    }
}
