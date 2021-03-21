package branscha.scripty.spec.map;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.junit.Assert.*;

public class DefResultMappingTest {

    @Test
    public void map()
    throws ResultMappingException {
        ResultMapping rm = new DefResultMapping("test");
        Context ctx = new BasicContext();

        rm.map("Olé", ctx);
        assertEquals("Olé", ctx.getBinding("test"));
    }
}