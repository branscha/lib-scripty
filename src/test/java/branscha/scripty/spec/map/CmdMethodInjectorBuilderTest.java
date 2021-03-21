package branscha.scripty.spec.map;

import branscha.scripty.annot.ScriptyBindingParam;
import branscha.scripty.annot.ScriptyParam;
import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import branscha.scripty.parser.Eval;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;

public class CmdMethodInjectorBuilderTest {

    @SuppressWarnings("unused")
    private static class Bean {
        public void noAnnotations1(Eval eval, Context ctx, Object[] args){ }
        public void noAnnotations2(Context ctx1, Eval eval, Context ctx2, Object[] args1, Object[] args2, Eval eval2){ }
        public void withScriptyParam(@ScriptyParam("test") Object param){}
        public void withScriptyBinding(@ScriptyBindingParam("test") Object param){}
        public void conflict(@ScriptyBindingParam("test") @ScriptyParam("test") Object param){}
    }

    @Test
    public void buildCmdMethodInjector_ByType()
    throws ArgMappingException {
        Eval eval = mock(Eval.class);
        Context ctx = mock(Context.class);
        Object[] args = new Object[0];

        Method[] methods = Bean.class.getMethods();

        // Standard argument list.
        CmdMethodInjector inj = CmdMethodInjectorBuilder.buildCmdMethodInjector(getMethodByName(methods, "noAnnotations1"), null);

        Object[] mapped = inj.map(eval, ctx, args);
        assertSame(eval, mapped[0]);
        assertSame(ctx, mapped[1]);
        assertSame(args, mapped[2]);

        // Contrived example where each type is provided multiple times.
        inj = CmdMethodInjectorBuilder.buildCmdMethodInjector(getMethodByName(methods, "noAnnotations2"), null);
        mapped = inj.map(eval, ctx, args);
        assertSame(ctx, mapped[0]);
        assertSame(eval, mapped[1]);
        assertSame(ctx, mapped[2]);
        assertSame(args, mapped[3]);
        assertSame(args, mapped[4]);
        assertSame(eval, mapped[5]);
    }

    @Test
    public void buildCmdMethodInjector_WithScriptyParam()
    throws ArgMappingException {
        Map<String, ArgMapping> argMappings = new HashMap<>();
        ArgMapping am = new ArrayIndexMapping(0);
        argMappings.put("test", am);

        Object[] args = new Object[]{"Bingo"};

        Method[] methods = Bean.class.getMethods();

        CmdMethodInjector inj = CmdMethodInjectorBuilder.buildCmdMethodInjector(getMethodByName(methods, "withScriptyParam"), argMappings);

        Object[] mapped = inj.map(null, null, args);
        assertEquals("Bingo", mapped[0]);
    }

    @Test
    public void buildCmdMethodInjector_WithScriptyBinding()
    throws ArgMappingException {

        Context ctx = new BasicContext();
        ctx.defBinding("test", "Hello");

        Method[] methods = Bean.class.getMethods();

        CmdMethodInjector inj = CmdMethodInjectorBuilder.buildCmdMethodInjector(getMethodByName(methods, "withScriptyBinding"), null);

        Object[] mapped = inj.map(null, ctx, null);
        assertEquals("Hello", mapped[0]);
    }

    @Test
    public void buildCmdMethodInjector_Conflict() {

        Method[] methods = Bean.class.getMethods();

        try {
            CmdMethodInjectorBuilder.buildCmdMethodInjector(getMethodByName(methods, "conflict"), null);
            fail("Builder should have failed.");
        }
        catch (ArgMappingException e) {
            assertThat(e.getMessage(), containsString("CmdMethodInjectorBuilder/010"));
        }
    }

    private Method getMethodByName(Method[] methods, String name) {
        for (Method method : methods) {
            if (name.equals(method.getName())) return method;
        }
        throw new IllegalArgumentException("Method not found");
    }
}