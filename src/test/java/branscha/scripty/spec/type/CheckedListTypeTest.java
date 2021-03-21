package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class CheckedListTypeTest {

    @Test
    public void getSpecName() {
        TypeSpec spec = new CheckedListType(IntegerType.INTEGER_TYPE, 0, 3);
        assertEquals("ListOf (Integer) 0 3", spec.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        TypeSpec spec = new CheckedListType(IntegerType.INTEGER_TYPE, 0, 3);
        assertEquals("ListOf (Integer) 0 3", spec.getSpecName());
        List<Integer> lst = Arrays.asList(1, 2, 3);
        Context ctx = new BasicContext();
        assertEquals(Arrays.asList(1, 2, 3), spec.guard(lst, ctx));
    }
}