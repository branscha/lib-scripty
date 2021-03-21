package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import java.math.BigInteger;

import static branscha.scripty.spec.type.BigIntegerType.BIGINTEGER_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class BigIntegerTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("BigInteger",BIGINTEGER_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals(BigInteger.valueOf(5), BIGINTEGER_TYPE.guard((short) 5, new BasicContext()));
        assertEquals(BigInteger.valueOf(7), BIGINTEGER_TYPE.guard(7, new BasicContext()));
        assertEquals(BigInteger.valueOf(11), BIGINTEGER_TYPE.guard("11", new BasicContext()));
        assertEquals(BigInteger.valueOf(13), BIGINTEGER_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            BIGINTEGER_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            BIGINTEGER_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

}