package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class InstanceOrBindingTest {

    @Test
    public void guard_Good()
    throws TypeSpecException {
        TypeSpec spec = new InstanceOrBinding(IntegerType.INTEGER_TYPE);
        Context ctx = new BasicContext();
        ctx.defBinding("test", 33);

        assertEquals(33, spec.guard("test", ctx));
        assertEquals(13, spec.guard("13", ctx));
    }

    @Test
    public void guard_Bad() {
        TypeSpec spec = new InstanceOrBinding(IntegerType.INTEGER_TYPE);
        Context ctx = new BasicContext();
        ctx.defBinding("test", 33);

        try {
            spec.guard("xxx", ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

    @Test
    public void getSpecName() {
        TypeSpec spec = new InstanceOrBinding(IntegerType.INTEGER_TYPE);
        assertEquals("InstanceOrBinding (Integer)", spec.getSpecName());
    }
}