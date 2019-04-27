package branscha.scripty.spec.type;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class OrTypeTest {
    private static class Plant {
    }

    private static class Animal {
    }


    @Test
    public void getSpecName() {
        OrType orType = new OrType(new TypeSpec[]{
                new InstanceType(Plant.class, "Plant", false),
                new InstanceType(Animal.class, "Animal", false)});

        assertEquals("OneOf (Plant) (Animal)", orType.getSpecName());
    }

    @Test
    public void guard_Good()
    throws TypeSpecException {
        OrType orType = new OrType(new TypeSpec[]{
                new InstanceType(Plant.class, "Plant", false),
                new InstanceType(Animal.class, "Animal", false)});
        Context ctx = new BasicContext();

        orType.guard(new Plant(), ctx);
        orType.guard(new Animal(), ctx);
    }

    @Test
    public void guard_Bad()
    throws TypeSpecException {
        OrType orType = new OrType(new TypeSpec[]{
                new InstanceType(Plant.class, "Plant", false),
                new InstanceType(Animal.class, "Animal", false)});
        Context ctx = new BasicContext();

        try {
            orType.guard(5, ctx);
            fail("Guard should have worked.");
        }
        catch (TypeSpecException e) {
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }
}