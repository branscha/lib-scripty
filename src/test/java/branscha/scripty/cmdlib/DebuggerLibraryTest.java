package branscha.scripty.cmdlib;

import branscha.scripty.ExtensionException;
import branscha.scripty.ProcessorException;
import branscha.scripty.ScriptyStreamProcessor;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;

import static junit.framework.TestCase.assertEquals;
import static org.hamcrest.CoreMatchers.containsString;
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

        lResult = scripty.process("dbg-eval (+ 1 2)");
        assertEquals(BigDecimal.valueOf(3), lResult);

        lResult = scripty.process("dbg-ctx");
        assertNotNull(lResult);

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

        scripty.process("dbg-terminate");
    }

    @Test
    public void dbgBpts()
    throws ProcessorException {
        scripty.process("dbg-addbreakpoint (bpt-func fie)");
        scripty.process("dbg-addbreakpoint (bpt-stack 10)");
        scripty.process("dbg-addbreakpoint (bpt-when (= $counter 10))");
        scripty.process("dbg-addbreakpoint (bpt-or (bpt-func a) (bpt-func b) (bpt-func c))");
        scripty.process("dbg-addbreakpoint (bpt-and (bpt-func 1) (bpt-func 2) (bpt-func 3))");
        scripty.process("dbg-addbreakpoint (bpt-not (bpt-func x))");

        Object lResult = scripty.process("dbg-breakpoints");
        assertEquals("BreakpointSet{breakpoints=[\n" +
                "    BreakpointFunc{name='bp0', funcName='fie', enabled=true},\n" +
                "    BreakpointStackdepth{name='bp1', depth=10, enabled=true},\n" +
                "    BreakpointWhen{name='bp2', breakExpression='(= $counter 10)', enabled=true},\n" +
                "    BreakpointOr{name='bp6', enabled=true, breakpoints=[\n" +
                "        BreakpointFunc{name='bp3', funcName='a', enabled=true},\n" +
                "        BreakpointFunc{name='bp4', funcName='b', enabled=true},\n" +
                "        BreakpointFunc{name='bp5', funcName='c', enabled=true}]},\n" +
                "    BreakpointAnd{name='bp10', enabled=true, breakpoints=[\n" +
                "        BreakpointFunc{name='bp7', funcName='1', enabled=true},\n" +
                "        BreakpointFunc{name='bp8', funcName='2', enabled=true},\n" +
                "        BreakpointFunc{name='bp9', funcName='3', enabled=true}]},\n" +
                "    BreakpointNot{name='bp12', enabled=true, breakpoint=BreakpointFunc{name='bp11', funcName='x', enabled=true}}]}", lResult.toString().trim());


        scripty.process("dbg-removebreakpoint bp6");
        scripty.process("dbg-removebreakpoint bp10");
        scripty.process("dbg-enablebreakpoint bp2 false");

        lResult = scripty.process("dbg-breakpoints");
        assertEquals("BreakpointSet{breakpoints=[\n" +
                "    BreakpointFunc{name='bp0', funcName='fie', enabled=true},\n" +
                "    BreakpointStackdepth{name='bp1', depth=10, enabled=true},\n" +
                "    BreakpointWhen{name='bp2', breakExpression='(= $counter 10)', enabled=false},\n" +
                "    BreakpointNot{name='bp12', enabled=true, breakpoint=BreakpointFunc{name='bp11', funcName='x', enabled=true}}]}", lResult.toString().trim());

        scripty.process("dbg-clearbreakpoints");

        lResult = scripty.process("dbg-breakpoints");
        assertEquals("BreakpointSet{breakpoints=[\n" +
                "]}", lResult.toString().trim());

    }

    @Test
    public void dbgExeption()
    throws ProcessorException {
        scripty.process("dbg-expr (dbg-raise aiai)");
        scripty.process("dbg-runresult");

        assertTrue((Boolean) scripty.process("dbg-exception?"));
        assertFalse((Boolean) scripty.process("dbg-moresteps?"));
        Object lResult = scripty.process("dbg-exception");
        assertThat(lResult.toString(), containsString("aiai"));
    }
}