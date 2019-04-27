package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class EnumTypeTest {

    @Test
    public void guard_Good()
    throws TypeSpecException {
        TypeSpec spec = new EnumType(Arrays.asList("uno", "duo", "tres"));
        Context ctx = new BasicContext();

        assertEquals("uno", spec.guard("uno", ctx));
        assertEquals("duo", spec.guard("duo", ctx));
        assertEquals("tres", spec.guard("tres", ctx));
    }

    @Test
    public void guard_Bad() {
        TypeSpec spec = new EnumType(Arrays.asList("uno", "duo", "tres"));
        Context ctx = new BasicContext();

        try {
            spec.guard("quattuor", ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/040"));
        }
    }

    @Test
    public void getSpecName() {
        TypeSpec spec = new EnumType(Arrays.asList("uno", "duo", "tres"));
        assertEquals("Enum uno duo tres", spec.getSpecName());
    }
}