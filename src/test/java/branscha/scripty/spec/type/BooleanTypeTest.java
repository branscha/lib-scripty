package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.BooleanType.BOOLEAN_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class BooleanTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("Boolean",BOOLEAN_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals(true, BOOLEAN_TYPE.guard((short) 5, new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard(7, new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard(13L, new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard("yes", new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard("on", new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard("true", new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard("ok", new BasicContext()));
        assertEquals(true, BOOLEAN_TYPE.guard("1", new BasicContext()));

        assertEquals(false, BOOLEAN_TYPE.guard("no", new BasicContext()));
        assertEquals(false, BOOLEAN_TYPE.guard("false", new BasicContext()));
        assertEquals(false, BOOLEAN_TYPE.guard("off", new BasicContext()));
        assertEquals(false, BOOLEAN_TYPE.guard("nok", new BasicContext()));
        assertEquals(false, BOOLEAN_TYPE.guard("0", new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            BOOLEAN_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            BOOLEAN_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}