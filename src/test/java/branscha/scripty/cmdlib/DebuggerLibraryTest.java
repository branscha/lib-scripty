package branscha.scripty.cmdlib;

import branscha.scripty.ExtensionException;
import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyStreamProcessor;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.*;

/**
 * The functionality is tested in {@link branscha.scripty.parser.TestEvalTrace}.
 * We only have to test the layer between the commands and the {@link branscha.scripty.parser.EvalTrace}.
 */
public class DebuggerLibraryTest {

    private ScriptyStreamProcessor scripty;

    @Before
    public void initialize()
    throws ExtensionException {
        scripty = new ScriptyStreamProcessor();
        scripty.addLibraryClasses(MathLibrary.class, DebuggerLibrary.class);
    }

    @Test
    public void dbgExpr()
    throws ProcessorException {
        scripty.process("dbg-expr (+ (* 2 3) 7)");
        Object lResult = scripty.process("dbg-stack");
        assertEquals("(+ (* 2 3) 7)", lResult.toString().trim());

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-step");
        lResult = scripty.process("dbg-stack");
        assertEquals("+\n" +
                "(+ (* 2 3) 7):0 ~ [null, null, null]", lResult.toString().trim());

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-stepover");

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-stepout");

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-back");

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-dropframe");

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-restart");

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        lResult = scripty.process("dbg-stack");
        assertEquals("(+ (* 2 3) 7):0 ~ [null, null, null]", lResult.toString().trim());

        assertFalse((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertTrue((Boolean) scripty.process("dbg-moresteps?"));

        scripty.process("dbg-run");

        assertTrue((Boolean) scripty.process("dbg-result?"));
        assertFalse((Boolean) scripty.process("dbg-exception?"));
        assertFalse((Boolean) scripty.process("dbg-moresteps?"));

        lResult = scripty.process("dbg-result");
        assertEquals(new BigDecimal(13), lResult);
    }
}