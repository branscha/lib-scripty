package branscha.scripty.spec.type;

import org.junit.Test;

import static org.junit.Assert.*;

public class AnyTypeTest {

    @Test
    public void getSpecName() {
        assertEquals("Any -n", new AnyType(true).getSpecName());
        assertEquals("Any", new AnyType(false).getSpecName());
    }
}