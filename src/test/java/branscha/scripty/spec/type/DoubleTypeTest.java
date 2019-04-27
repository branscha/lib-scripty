package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.DoubleType.DOUBLE_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class DoubleTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("Double",DOUBLE_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals((double) 5, DOUBLE_TYPE.guard((short) 5, new BasicContext()));
        assertEquals((double) 7, DOUBLE_TYPE.guard(7, new BasicContext()));
        assertEquals((double) 11, DOUBLE_TYPE.guard("11", new BasicContext()));
        assertEquals((double) 13, DOUBLE_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            DOUBLE_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            DOUBLE_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

}