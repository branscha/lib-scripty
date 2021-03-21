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
import org.junit.Before;
import org.junit.Test;

import java.util.*;

import static junit.framework.TestCase.*;

public class TestMapLibrary {
    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(MapLibrary.class);
    }

    @Test
    public void createMap1()
    throws ProcessorException {
        Object lResult = scripty.process("map-create");
        assertTrue(lResult instanceof java.util.Map);
        assertTrue(((Map) lResult).isEmpty());
    }

    @Test
    public void createMap2()
    throws ProcessorException {
        Object lResult = scripty.process("map-create a=uno b=duo c=tres d=quattuor");

        assertTrue(lResult instanceof java.util.Map);
        Map lResultMap = (Map) lResult;

        assertEquals(4, lResultMap.size());
        assertEquals(lResultMap.get("a"), "uno");
        assertEquals(lResultMap.get("b"), "duo");
        assertEquals(lResultMap.get("c"), "tres");
        assertEquals(lResultMap.get("d"), "quattuor");
    }

    @Test
    public void createMap3()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        Object lResult = scripty.process("map-create $map");

        assertTrue(lResult instanceof java.util.Map);
        Map lResultMap = (Map) lResult;

        assertEquals(4, lResultMap.size());
        assertEquals(lResultMap.get("a"), "uno");
        assertEquals(lResultMap.get("b"), "duo");
        assertEquals(lResultMap.get("c"), "tres");
        assertEquals(lResultMap.get("d"), "quattuor");
    }

    @Test(expected = ProcessorException.class)
    public void createMap4()
    throws ProcessorException {
        scripty.process("map-create '()");
        fail();
    }

    @Test
    public void createMap5()
    throws ProcessorException {
        Object lResult = scripty.process("map-create a b c d");

        assertTrue(lResult instanceof java.util.Map);
        Map lResultMap = (Map) lResult;

        assertEquals(4, lResultMap.size());
        assertNull(lResultMap.get("a"));
        assertNull(lResultMap.get("b"));
        assertNull(lResultMap.get("c"));
        assertNull(lResultMap.get("d"));
    }

    @Test
    public void testMap1()
    throws ProcessorException {
        Object lResult = scripty.process("map? (map-create)");
        assertTrue((Boolean) lResult);
    }

    @Test
    public void testMap2()
    throws ProcessorException {
        Object lResult = scripty.process("map? $null");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void testMap3()
    throws ProcessorException {
        Object lResult = scripty.process("map? abc");
        assertFalse((Boolean) lResult);
    }

    @Test
    public void testMapSet1()
    throws ProcessorException {
        Map lMap = new HashMap();
        scripty.getContext().defBinding("map", lMap);

        scripty.process("map-set $map key 123");
        assertEquals(1, lMap.size());
        assertTrue(lMap.containsKey("key"));
        assertEquals("123", lMap.get("key"));
    }

    @Test
    public void testMapGet1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        assertEquals("uno", scripty.process("map-get $map a"));
        assertEquals("duo", scripty.process("map-get $map b"));
        assertEquals("tres", scripty.process("map-get $map c"));
        assertEquals("quattuor", scripty.process("map-get $map d"));
    }

    @Test(expected = ProcessorException.class)
    public void testMapGet2()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        scripty.process("map-get $map x");
        fail();
    }

    @Test
    public void testMapKey1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        assertTrue((Boolean) scripty.process("map-key? $map a"));
        assertTrue((Boolean) scripty.process("map-key? $map b"));
        assertTrue((Boolean) scripty.process("map-key? $map c"));
        assertTrue((Boolean) scripty.process("map-key? $map d"));
        assertFalse((Boolean) scripty.process("map-key? $map x"));
    }

    @Test
    public void testMapKeys1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        Set lKeys = (Set) scripty.process("map-keys $map");
        assertEquals(4, lKeys.size());
        assertTrue(lKeys.containsAll(Arrays.asList("a", "b", "c", "d")));
    }

    @Test
    public void testMapValues1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        Collection lValues = (Collection) scripty.process("map-values $map");
        assertEquals(4, lValues.size());
        assertTrue(lValues.containsAll(Arrays.asList("uno", "duo", "tres", "quattuor")));
    }

    @Test
    public void testClear1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        assertEquals(4, lMap.size());
        scripty.process("map-clear $map");
        assertEquals(0, lMap.size());
    }

    @Test
    public void testSize1()
    throws ProcessorException {
        Map<String, Object> lMap = new HashMap<>();
        lMap.put("a", "uno");
        lMap.put("b", "duo");
        lMap.put("c", "tres");
        lMap.put("d", "quattuor");
        scripty.getContext().defBinding("map", lMap);

        assertEquals(scripty.process("map-size $map"), 4);
        assertEquals(scripty.process("map-size (map-create)"), 0);
    }
}
