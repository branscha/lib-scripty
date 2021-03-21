package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static branscha.scripty.spec.type.ByteType.BYTE_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class ByteTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("Byte",BYTE_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals((byte) 5, BYTE_TYPE.guard((short) 5, new BasicContext()));
        assertEquals((byte) 7, BYTE_TYPE.guard(7, new BasicContext()));
        assertEquals((byte) 11, BYTE_TYPE.guard("11", new BasicContext()));
        assertEquals((byte) 13, BYTE_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            BYTE_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            BYTE_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

}