package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class IntegerRangeTypeTest {

    @Test
    public void getSpecName() {
        TypeSpec range = new IntegerRangeType(5, 10);
        assertEquals("IntegerRange --min=5 --max=10", range.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        TypeSpec range = new IntegerRangeType(5, 10);
        assertEquals("IntegerRange --min=5 --max=10", range.getSpecName());
        Context ctx = new BasicContext();

        for (int i = 5; i < 11; i++) assertEquals(i, range.guard(i, ctx));
    }

    @Test
    public void guard_Bad()
    throws TypeSpecException {
        TypeSpec range = new IntegerRangeType(5, 10);
        assertEquals("IntegerRange --min=5 --max=10", range.getSpecName());
        Context ctx = new BasicContext();

        try {
            range.guard(4, ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("IntegerRangeType/010"));
        }

        try {
            range.guard(11, ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("IntegerRangeType/010"));
        }
    }
}