package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.ShortType.SHORT_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class ShortTypeTest {

    @Test
    public void getSpecName() {
        assertEquals("Short",SHORT_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals((short) 5, SHORT_TYPE.guard((short) 5, new BasicContext()));
        assertEquals((short) 7, SHORT_TYPE.guard(7, new BasicContext()));
        assertEquals((short) 11, SHORT_TYPE.guard("11", new BasicContext()));
        assertEquals((short) 13, SHORT_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            SHORT_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            SHORT_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}