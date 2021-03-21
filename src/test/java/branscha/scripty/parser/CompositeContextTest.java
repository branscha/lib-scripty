package branscha.scripty.parser;

import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.*;

public class CompositeContextTest {

    @Test
    public void testContext()
    throws CommandException {

        Context base = new BasicContext();
        base.defBinding("address", "north pole");
        Context child = new BasicContext();
        CompositeContext ctx = new CompositeContext(child, base);

        // Already bound in parent.
        assertTrue(ctx.isBound("address"));
        assertEquals("north pole", base.getBinding("address"));

        // Modifying it must modify the original binding.
        ctx.setBinding("address", "Here");
        assertEquals("Here", base.getBinding("address"));

        // Defining address again makes a new binding in the child context.
        ctx.defBinding("address", "south pole");
        assertEquals("Here", base.getBinding("address"));
        assertEquals("south pole", child.getBinding("address"));

        // Unbind the most recent one
        ctx.removeBinding("address");
        // The original one must remain intact.
        assertEquals("Here", ctx.getBinding("address"));
        assertTrue(ctx.isBound("address"));
        assertTrue(base.isBound("address"));
        assertFalse(child.isBound("address"));

        // Define a new binding.
        ctx.defBinding("name", "Bruno");
        assertEquals("Bruno", ctx.getBinding("name"));
        assertTrue(ctx.isBound("name"));

        // It must be unknown in base.
        assertNull(base.getBinding("name"));

        assertEquals(base, ctx.getRootContext());

        assertEquals("address = Here\n", base.toString());
        assertEquals("name = Bruno\n", child.toString());
        assertEquals("name = Bruno\n" +
                "----------\n" +
                "address = Here\n", ctx.toString());

        Map result = ctx.dumpBindings();
        assertTrue(result.containsKey("name"));
        assertTrue(result.containsKey("address"));
    }

    @Test(expected = CommandException.class)
    public void setUndefined()
    throws CommandException {

        Context base = new BasicContext();
        Context child = new BasicContext();
        CompositeContext ctx = new CompositeContext(child, base);

        ctx.setBinding("ai", "does not exist");
    }
}