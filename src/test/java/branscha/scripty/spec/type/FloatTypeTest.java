package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.FloatType.FLOAT_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class FloatTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("Float",FLOAT_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals((float) 5, FLOAT_TYPE.guard((short) 5, new BasicContext()));
        assertEquals((float) 7, FLOAT_TYPE.guard(7, new BasicContext()));
        assertEquals((float) 11, FLOAT_TYPE.guard("11", new BasicContext()));
        assertEquals((float) 13, FLOAT_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            FLOAT_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            FLOAT_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}