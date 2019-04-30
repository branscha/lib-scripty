package branscha.scripty.spec.map;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class SetResultMappingTest {
    @Test
    public void map_Good()
    throws ResultMappingException {
        ResultMapping rm = new SetResultMapping("exists");
        Context ctx = new BasicContext();
        ctx.defBinding("exists", "one");

        rm.map("two", ctx);
        assertEquals("two", ctx.getBinding("exists"));
    }

    @Test
    public void map_Bad() {
        ResultMapping rm = new SetResultMapping("test");
        Context ctx = new BasicContext();

        try {
            rm.map("Olé", ctx);
            fail("Mapping should fail because binding does not exist.");
        }
        catch (ResultMappingException e) {
            assertThat(e.getMessage(), containsString("SetResultMapping/010"));
        }
    }

}