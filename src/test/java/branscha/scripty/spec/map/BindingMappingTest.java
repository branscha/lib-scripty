package branscha.scripty.spec.map;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class BindingMappingTest {

    @Test
    public void map()
    throws ArgMappingException {
        ArgMapping bm = new BindingMapping("test", true);
        Context ctx = new BasicContext();

        try {
            bm.map(null, ctx, null);
            fail("Mapper should have thrown exception.");
        }
        catch (ArgMappingException e) {
            assertThat(e.getMessage(), containsString("BindingMapping/010"));
        }

        ctx.defBinding("test", "abc123");
        assertEquals("abc123", bm.map(null, ctx, null));

        bm = new BindingMapping("does-not-exist", false);
        assertNull(bm.map(null, ctx, null));
    }
}