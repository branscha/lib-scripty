package branscha.scripty.spec.map;

import branscha.scripty.parser.Context;
import branscha.scripty.parser.Eval;
import org.junit.Test;

import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;

public class NullMappingTest {
    @Test
    public void map()
    throws ArgMappingException {
        ArgMapping m = new NullMapping();

        Eval eval = mock(Eval.class);
        Context ctx = mock(Context.class);
        Object[] args = new Object[0];

        Object arg = m.map(eval, ctx, args);
        assertNull(arg);
    }
}