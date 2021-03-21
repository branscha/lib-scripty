package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class InstanceTypeTest {

    @Test
    public void guard_Good()
    throws TypeSpecException {
        InstanceType intType = new InstanceType(Integer.class, true);
        Context ctx = new BasicContext();
        assertEquals(13, intType.guard(13, ctx));
        assertNull(intType.guard(null, ctx));

        InstanceType strType = new InstanceType(String.class, false);
        strType.guard("Hello", ctx);

        InstanceType numType = new InstanceType(Number.class, false);
        assertEquals(17, numType.guard(17, ctx));
        assertEquals(19L, numType.guard(19L, ctx));
    }

    @Test
    public void guard_Bad() {
        InstanceType intType = new InstanceType(Integer.class, true);
        Context ctx = new BasicContext();

        try {
            intType.guard("This is a string", ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }

        InstanceType strType = new InstanceType(String.class, false);

        try {
            strType.guard(null, ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("InstanceType/010"));
        }
    }

    @Test
    public void getSpecName() {
        // Name explicitly provided.
        assertEquals("myType", new InstanceType(Integer.class, "myType", true).getSpecName());

        // Generated name.
        assertEquals("Instance java.lang.Integer -n", new InstanceType(Integer.class, true).getSpecName());
        assertEquals("Instance java.lang.Integer", new InstanceType(Integer.class, false).getSpecName());
    }
}