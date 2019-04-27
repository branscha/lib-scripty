package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class StringTypeTest {

    @Test
    public void getSpecName() {
        assertEquals("String", StringType.STRING_TYPE.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        StringType.STRING_TYPE.guard("ok", new BasicContext());
    }

    @Test
    public void guard_Bad()
    throws TypeSpecException {
        try {
            StringType.STRING_TYPE.guard(13, new BasicContext());
            fail("Expected guard to fail.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}