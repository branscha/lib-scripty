package branscha.scripty.spec.map;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CmdMethodInjectorTest {

    @Test
    public void map()
    throws ArgMappingException {
        Object[] args = new Object[]{"uno", "duo"};
        Context ctx = new BasicContext();
        ctx.defBinding("test", "tres");

        CmdMethodInjector injector = new CmdMethodInjector();
        injector.addArgMapping(new ArrayIndexMapping(0));
        injector.addArgMapping(new BindingMapping("test", true));
        injector.addArgMapping(new ArrayIndexMapping(1));

        Object[] mapped = injector.map(null, ctx, args);
        assertEquals("uno", mapped[0]);
        assertEquals("tres", mapped[1]);
        assertEquals("duo", mapped[2]);
    }
}