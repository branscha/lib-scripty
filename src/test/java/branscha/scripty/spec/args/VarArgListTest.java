package branscha.scripty.spec.args;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import branscha.scripty.parser.Pair;
import branscha.scripty.spec.type.IntegerType;
import branscha.scripty.spec.type.StringType;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

/**
 * We will not repeat the tests for fixed and named arguments here, they can be found in the {@link StdArgListTest}.
 * The code is shared between the types of argument lists. We will concentrate on the variable arguments and
 * the combination of all types of arguments.
 */
public class VarArgListTest {

    @Test
    public void varOnly_Empty()
    throws ArgSpecException {
        VarArgList varOnly = new VarArgList.Builder()
                .addVarArg(new VarArg(IntegerType.INTEGER_TYPE), 0, 0)
                .build();
        Object[] args = new Object[]{"cmd"};
        Context ctx = new BasicContext();
        assertNotNull(varOnly.guard(args, ctx));
    }

    @Test
    public void varOnly_EmptyTooManyArgs()
    throws ArgSpecException {
        VarArgList varOnly = new VarArgList.Builder()
                .addVarArg(new VarArg(IntegerType.INTEGER_TYPE), 0, 0)
                .build();
        Object[] args = new Object[]{"cmd", "1"};
        Context ctx = new BasicContext();
        try {
            varOnly.guard(args, ctx);
            fail("Guard should have worked");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("VarArgList/030"));
        }
    }

    @Test
    public void varOnly_TwoTooFewArgs()
    throws ArgSpecException {
        VarArgList varOnly = new VarArgList.Builder()
                .addVarArg(new VarArg(IntegerType.INTEGER_TYPE), 1, 2)
                .build();
        Object[] args = new Object[]{"cmd"};
        Context ctx = new BasicContext();
        try {
            varOnly.guard(args, ctx);
            fail("Guard should have worked");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("VarArgList/020"));
        }
    }

    @Test
    public void varOnly_TwoTooManyArgs()
    throws ArgSpecException {
        VarArgList varOnly = new VarArgList.Builder()
                .addVarArg(new VarArg(IntegerType.INTEGER_TYPE), 1, 2)
                .build();
        Object[] args = new Object[]{"cmd", "1", "2", "3"};
        Context ctx = new BasicContext();
        try {
            varOnly.guard(args, ctx);
            fail("Guard should have worked");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("VarArgList/030"));
        }
    }

    @Test
    public void mixed_Good()
    throws ArgSpecException {
        VarArgList mixed = new VarArgList.Builder()
                .addVarArg(new VarArg(IntegerType.INTEGER_TYPE), -1, -1)
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addNamed(new NamedArg("city", "c", StringType.STRING_TYPE, "London", true))
                .build();
        Object[] args = new Object[]{"cmd",new Pair("--city","Bruges"), "Bruno", "1", "2", "3"};
        Context ctx = new BasicContext();
        Object[] guarded = mixed.guard(args, ctx);

        // Our only fixed variable.
        assertEquals("Bruno", guarded[1]);
        // A bit counterintuitive, the named is next.
        assertEquals("Bruges", guarded[2]);
        // Now the var arg.
        assertEquals(Integer.valueOf(1), guarded[3]);
        assertEquals(Integer.valueOf(2), guarded[4]);
        assertEquals(Integer.valueOf(3), guarded[5]);

    }

}