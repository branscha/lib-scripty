package branscha.scripty.spec.type;

import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyCapable;
import branscha.scripty.ScriptyStreamProcessor;
import branscha.scripty.parser.Context;
import branscha.scripty.repl.ReplEngine;
import org.junit.Before;
import org.junit.Test;
import org.omg.CORBA.Any;

import static org.junit.Assert.*;

/**
 * These tests must only check if the language produces the correct {@link TypeSpec} instances.
 * The functionality of the specs themselves is tested in their own unit test.
 */
public class TypeLanguageLibTest {

    private ScriptyStreamProcessor scripty;

    @Before
    public void setUp()
    throws Exception {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(TypeLanguageLib.class);
    }

    @Test
    public void bigDecimalType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("BigDecimal");
        assertTrue(spec instanceof BigDecimalType);
    }

    @Test
    public void integerType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Integer");
        assertTrue(spec instanceof IntegerType);
    }

    @Test
    public void bigIntegerType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("BigInteger");
        assertTrue(spec instanceof BigIntegerType);
    }

    @Test
    public void booleanType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Boolean");
        assertTrue(spec instanceof BooleanType);
    }

    @Test
    public void byteType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Byte");
        assertTrue(spec instanceof ByteType);
    }

    @Test
    public void listOfType() {
    }

    @Test
    public void doubleType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Double");
        assertTrue(spec instanceof DoubleType);
    }

    @Test
    public void floatType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Float");
        assertTrue(spec instanceof FloatType);
    }

    @Test
    public void instanceOrBindingType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("InstanceOrBinding (Integer)");
        assertTrue(spec instanceof InstanceOrBinding);
        assertEquals("InstanceOrBinding (Integer)", spec.getSpecName());
    }

    @Test
    public void instanceType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Instance java.lang.Integer");
        assertTrue(spec instanceof InstanceType);
        assertEquals("Instance java.lang.Integer", spec.getSpecName());
    }

    @Test
    public void anyType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Any --nullAllowed=false");
        assertTrue(spec instanceof AnyType);
        assertEquals("Any", spec.getSpecName());
    }

    @Test
    public void integerRangeType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("IntegerRange --min=5 --max=10");
        assertTrue(spec instanceof IntegerRangeType);
        assertEquals("IntegerRange --min=5 --max=10", spec.getSpecName());

        spec = (TypeSpec) scripty.process("IntegerRange --max=100");
        assertTrue(spec instanceof IntegerRangeType);
        assertEquals("IntegerRange --min=-2147483648 --max=100", spec.getSpecName());

        spec = (TypeSpec) scripty.process("IntegerRange --min=0");
        assertTrue(spec instanceof IntegerRangeType);
        assertEquals("IntegerRange --min=0 --max=2147483647", spec.getSpecName());
    }

    @Test
    public void longType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Long");
        assertTrue(spec instanceof LongType);
    }

    @Test
    public void shortType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Short");
        assertTrue(spec instanceof ShortType);
    }

    @Test
    public void stringType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("String");
        assertTrue(spec instanceof StringType);
    }

    @Test
    public void oneOfType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("OneOf (Short) (String) (Instance java.lang.Integer)");
        assertTrue(spec instanceof OrType);
        assertEquals("OneOf (Short) (String) (Instance java.lang.Integer)", spec.getSpecName());
    }

    public static class MySpec implements TypeSpec {
        @Override
        public Object guard(Object arg, Context ctx)
        throws TypeSpecException {
            return arg;
        }

        @Override
        public String getSpecName() {
            return "MySpec";
        }
    }

    @Test
    public void customSpec()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Custom branscha.scripty.spec.type.TypeLanguageLibTest$MySpec");
        assertTrue(spec instanceof MySpec);
        assertEquals("MySpec", spec.getSpecName());
    }

    @Test
    public void enumType()
    throws ProcessorException {
        TypeSpec spec = (TypeSpec) scripty.process("Enum red green blue  orange");
        assertTrue(spec instanceof EnumType);
        assertEquals("Enum red green blue orange", spec.getSpecName());
    }
}