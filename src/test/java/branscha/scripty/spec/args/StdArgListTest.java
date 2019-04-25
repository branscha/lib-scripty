package branscha.scripty.spec.args;

import branscha.scripty.parser.BasicContext;
import branscha.scripty.parser.Context;
import branscha.scripty.parser.Pair;
import branscha.scripty.spec.type.StringType;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class StdArgListTest {

    @Test
    public void emptyArgList_Good()
    throws ArgSpecException {
        StdArgList emptyArgList = new StdArgList.Builder().build();
        Object[] args = new Object[]{"cmd"};
        Context ctx = new BasicContext();
        assertNotNull(emptyArgList.guard(args, ctx));
    }

    @Test
    public void emptyArgList_Bad() {
        StdArgList emptyArgList = new StdArgList.Builder().build();
        Object[] args = new Object[]{"cmd", "one"};
        Context ctx = new BasicContext();
        try {
            emptyArgList.guard(args, ctx);
            fail("Expected the argument list guard to fail.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("StdArgList/020"));
        }
    }

    @Test
    public void fixedOnly_Good()
    throws ArgSpecException {
        StdArgList fixedOnly = new StdArgList.Builder()
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .build();
        Object[] args = new Object[]{"cmd", "uno", "duo", "tres"};
        Context ctx = new BasicContext();
        assertNotNull(fixedOnly.guard(args, ctx));
    }

    @Test
    public void fixedOnly_BadTooManyArgs() {
        StdArgList fixedOnly = new StdArgList.Builder()
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .build();
        Object[] args = new Object[]{"cmd", "uno", "duo", "tres"};
        Context ctx = new BasicContext();
        try {
            fixedOnly.guard(args, ctx);
            fail("Expected the guard to fail.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("StdArgList/020"));
        }
    }

    @Test
    public void fixedOnly_BadTooFewArguments() {
        StdArgList fixedOnly = new StdArgList.Builder()
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .build();
        Object[] args = new Object[]{"cmd", "uno", "duo"};
        Context ctx = new BasicContext();
        try {
            fixedOnly.guard(args, ctx);
            fail("Expected the guard to fail.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("AbstractArgList/030"));
        }
    }

    @Test
    public void fixedOnly_BadType() {
        StdArgList fixedOnly = new StdArgList.Builder()
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .build();
        Object[] args = new Object[]{"cmd", 1};
        Context ctx = new BasicContext();
        try {
            fixedOnly.guard(args, ctx);
            fail("Expected the guard to fail.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("FixedArg/020"));
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

    @Test
    public void optionalOnly_Good()
    throws ArgSpecException {
        StdArgList optOnly = new StdArgList.Builder()
                .addOptional(new OptionalArg(StringType.STRING_TYPE, "val1"))
                .addOptional(new OptionalArg(StringType.STRING_TYPE, "val2"))
                .addOptional(new OptionalArg(StringType.STRING_TYPE, "val3"))
                .build();

        // All arguments provided.
        Object[] args = new Object[]{"cmd", "a", "b", "c"};
        Context ctx = new BasicContext();
        Object[] guarded = optOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "a", "b", "c"}, guarded);

        // Some
        args = new Object[]{"cmd", "a"};
        guarded = optOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "a", "val2", "val3"}, guarded);

        // None.
        args = new Object[]{"cmd"};
        guarded = optOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "val1", "val2", "val3"}, guarded);
    }

    @Test
    public void optionalOnly_BadType() {
        StdArgList optOnly = new StdArgList.Builder()
                .addOptional(new OptionalArg(StringType.STRING_TYPE, "val1"))
                .build();

        // All arguments provided.
        Object[] args = new Object[]{"cmd", 13};
        Context ctx = new BasicContext();

        try {
            optOnly.guard(args, ctx);
            fail("Expected the guard to fail.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("OptionalArg/010"));
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

    @Test
    public void namedOnly_Good()
    throws ArgSpecException {
        StdArgList namedOnly = new StdArgList.Builder()
                .addNamed(new NamedArg("a1", StringType.STRING_TYPE, "val1", true))
                .addNamed(new NamedArg("a2", StringType.STRING_TYPE, "val2", true))
                .addNamed(new NamedArg("a3", StringType.STRING_TYPE, "val3", true))
                .build();

        // All arguments provided.
        Object[] args = new Object[]{"cmd", new Pair("a1", "one"), new Pair("a2","two"), new Pair("a3", "three")};
        Context ctx = new BasicContext();
        Object[] guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "one", "two", "three"}, guarded);

        // Last one missing.
        args = new Object[]{"cmd", new Pair("a1", "one"), new Pair("a2","two")};
        guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "one", "two", "val3"}, guarded);

        // All missing.
        args = new Object[]{"cmd"};
        guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "val1", "val2", "val3"}, guarded);
    }

    @Test
    public void namedOnly_Bad() {
        StdArgList namedOnly = new StdArgList.Builder()
                .addNamed(new NamedArg("a1", StringType.STRING_TYPE, "val1", true))
                .addNamed(new NamedArg("a2", StringType.STRING_TYPE, "val2", false))
                .addNamed(new NamedArg("a3", StringType.STRING_TYPE, "val3", true))
                .build();

        // Second one is required ...
        Object[] args = new Object[]{"cmd", new Pair("a1", "one")};
        Context ctx = new BasicContext();

        try {
            namedOnly.guard(args, ctx);
            fail("Guard should have taken action.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("NamedArg/020"));
        }
    }

    @Test
    public void namedOnly_BadTooMany() {
        StdArgList namedOnly = new StdArgList.Builder()
                .addNamed(new NamedArg("a1", StringType.STRING_TYPE, "val1", true))
                .build();

        // Added an argument too much ...
        Object[] args = new Object[]{"cmd", new Pair("a1", "one"), new Pair("a2", "one")};
        Context ctx = new BasicContext();

        try {
            namedOnly.guard(args, ctx);
            fail("Guard should have taken action.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("AbstractArgList/020"));
        }
    }

    @Test
    public void namedOnly_BadType() {
        StdArgList namedOnly = new StdArgList.Builder()
                .addNamed(new NamedArg("a1", StringType.STRING_TYPE, "val1", true))
                .build();

        // Second one is required ...
        Object[] args = new Object[]{"cmd", new Pair("a1", 17)};
        Context ctx = new BasicContext();

        try {
            namedOnly.guard(args, ctx);
            fail("Guard should have taken action.");
        }
        catch (ArgSpecException e) {
            assertThat(e.getMessage(), containsString("NamedArg/010"));
            assertThat(e.getMessage(), containsString("TypeUtil/020"));
        }
    }

    @Test
    public void mixed_Good()
    throws ArgSpecException {
        StdArgList namedOnly = new StdArgList.Builder()
                .addFixed(new FixedArg(StringType.STRING_TYPE))
                .addOptional(new OptionalArg(StringType.STRING_TYPE, "val2"))
                .addNamed(new NamedArg("a3", StringType.STRING_TYPE, "val3", true))
                .build();

        // All provided.
        Object[] args = new Object[]{"cmd", "x", "y", new Pair("a3", "z")};
        Context ctx = new BasicContext();
        Object[] guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "x", "y", "z"}, guarded);

        // Omit optional and named.
        args = new Object[]{"cmd", "x"};
        guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "x", "val2", "val3"}, guarded);

        // Omit optional.
        args = new Object[]{"cmd", "x", new Pair("a3", "z")};
        guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "x", "val2", "z"}, guarded);

        //  Omit named.
        args = new Object[]{"cmd", "x", "y"};
        guarded = namedOnly.guard(args, ctx);
        assertNotNull(guarded);
        assertArrayEquals(new Object[]{"cmd", "x", "y", "val3"}, guarded);
    }

}