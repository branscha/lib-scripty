package branscha.scripty.spec.map;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;

public class PartialMappingTest {

    @Test
    public void map() {
        Object[] args = new Object[]{0, 1, 2, 3, 4, 5};
        assertArrayEquals(new Object[]{1, 2, 3, 4, 5}, (Object[]) new PartialMapping(1, -1).map(null, null, args));
        assertArrayEquals(new Object[]{2, 3, 4, 5}, (Object[]) new PartialMapping(2, -1).map(null, null, args));
        assertArrayEquals(new Object[]{}, (Object[]) new PartialMapping(3, 0).map(null, null, args));
        assertArrayEquals(new Object[]{3}, (Object[]) new PartialMapping(3, 1).map(null, null, args));
        assertArrayEquals(new Object[]{5}, (Object[]) new PartialMapping(5, 1).map(null, null, args));
        assertArrayEquals(new Object[]{5}, (Object[]) new PartialMapping(5, -1).map(null, null, args));
    }
}