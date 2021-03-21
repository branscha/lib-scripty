package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.IntegerType.INTEGER_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class IntegerTypeTest {

    @Test
    public void getSpecName() {
        assertEquals("Integer",INTEGER_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals(5, INTEGER_TYPE.guard((short) 5, new BasicContext()));
        assertEquals(7, INTEGER_TYPE.guard(7, new BasicContext()));
        assertEquals(11, INTEGER_TYPE.guard("11", new BasicContext()));
        assertEquals(13, INTEGER_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            INTEGER_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            INTEGER_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}