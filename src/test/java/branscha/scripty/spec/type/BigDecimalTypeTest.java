package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import java.math.BigDecimal;

import static branscha.scripty.spec.type.BigDecimalType.BIGDECIMAL_TYPE;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class BigDecimalTypeTest {
    @Test
    public void getSpecName() {
        assertEquals("BigDecimal",BIGDECIMAL_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        assertEquals(BigDecimal.valueOf(5), BIGDECIMAL_TYPE.guard((short) 5, new BasicContext()));
        assertEquals(BigDecimal.valueOf(7), BIGDECIMAL_TYPE.guard(7, new BasicContext()));
        assertEquals(BigDecimal.valueOf(11), BIGDECIMAL_TYPE.guard("11", new BasicContext()));
        assertEquals(BigDecimal.valueOf(13), BIGDECIMAL_TYPE.guard(13L, new BasicContext()));
    }

    @Test
    public void guard_Bad() {
        try {
            BIGDECIMAL_TYPE.guard("abc 123", new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        try {
            BIGDECIMAL_TYPE.guard(new Object(), new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

}