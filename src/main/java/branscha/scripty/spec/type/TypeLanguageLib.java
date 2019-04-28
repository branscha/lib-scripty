package branscha.scripty.spec.type;

import branscha.scripty.annot.ScriptyCommand;
import branscha.scripty.parser.CommandException;
import branscha.scripty.parser.Context;
import branscha.scripty.spec.args.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static branscha.scripty.spec.type.BigDecimalType.BIGDECIMAL_TYPE;
import static branscha.scripty.spec.type.BigIntegerType.BIGINTEGER_TYPE;
import static branscha.scripty.spec.type.BooleanType.BOOLEAN_TYPE;
import static branscha.scripty.spec.type.ByteType.BYTE_TYPE;
import static branscha.scripty.spec.type.DoubleType.DOUBLE_TYPE;
import static branscha.scripty.spec.type.FloatType.FLOAT_TYPE;
import static branscha.scripty.spec.type.IntegerType.INTEGER_TYPE;
import static branscha.scripty.spec.type.LongType.LONG_TYPE;
import static branscha.scripty.spec.type.ShortType.SHORT_TYPE;
import static branscha.scripty.spec.type.StringType.STRING_TYPE;

/**
 * The Scripty Type Language which is used in the {@link branscha.scripty.annot.ScriptyArg} annotations to
 * describe the argument type.
 */
public class TypeLanguageLib {

    private static TypeSpec SPEC_TYPE = new InstanceType(TypeSpec.class, false);

    private static NamedArg NAMED_MINLEN = new NamedArg("minLength", new IntegerRangeType(-1, Integer.MAX_VALUE), -1, true);
    private static NamedArg NAMED_MAXLEN = new NamedArg("maxLength", new IntegerRangeType(-1, Integer.MAX_VALUE), -1, true);
    private static NamedArg NAMED_MIN = new NamedArg("min", new IntegerRangeType(Integer.MIN_VALUE, Integer.MAX_VALUE), Integer.MIN_VALUE, true);
    private static NamedArg NAMED_MAX = new NamedArg("max", new IntegerRangeType(Integer.MIN_VALUE, Integer.MAX_VALUE), Integer.MAX_VALUE, true);
    private static NamedArg NAMED_NULLALLOWED = new NamedArg("nullAllowed", BOOLEAN_TYPE, false, true);

    private static FixedArg FIXED_TYPESPEC = new FixedArg(SPEC_TYPE);
    private static FixedArg FIXED_CLASSNAME = new FixedArg(STRING_TYPE);

    private static VarArg VAR_STRING = new VarArg(STRING_TYPE);

    private static final FixedArg[] NO_FIXED = {};
    private static final NamedArg[] NO_NAMED = {};
    private static final OptionalArg[] NO_OPTIONAL = {};

    /**
     * An empty arguments list. It is used by many of the Scripty type language commands: BigDecimal,
     * Integer, BigInteger, Boolean, Byte, Double, Float, Long, Short and String. These commands do not accept
     * any arguments.
     */
    private static StdArgList NOARGS_ARGLIST = new StdArgList(NO_FIXED, NO_OPTIONAL, NO_NAMED);

    /**
     * Argument list for {@link #listOfType(Context, Object[])}. It accepts a type for the list items and a length range.
     * Example:
     * <pre>
     *     type="ListOf (Integer) minLength=0 maxLength=20"
     * </pre>
     */
    private static StdArgList LISTOF_ARGLIST = new StdArgList(new FixedArg[]{FIXED_TYPESPEC}, NO_OPTIONAL, new NamedArg[]{NAMED_MINLEN, NAMED_MAXLEN});

    /**
     * Argument list for {@link #instanceOrBindingType(Context, Object[])}. It accepts a type for the instance.
     * Example:
     * <pre>
     *     type="InstanceOrBinding (Integer)"
     * </pre>
     */
    private static StdArgList INSTANCEORBINDING_ARGLIST = new StdArgList(new FixedArg[]{FIXED_TYPESPEC}, NO_OPTIONAL, NO_NAMED);

    /**
     * Argument list for {@link #instanceType(Context, Object[])}. It accepts a classname for the instance.
     * Example:
     * <pre>
     *     type="Instance branscha.MyClass nullAllowed=true"
     * </pre>
     */
    private static StdArgList INSTANCE_ARGLIST = new StdArgList(new FixedArg[]{FIXED_CLASSNAME}, NO_OPTIONAL, new NamedArg[]{NAMED_NULLALLOWED});

    /**
     * Argument list for {@link #anyType(Context, Object[])}. It accepts a flag to indicate if null values are allowed
     * in the value set or not.
     *
     * Example:
     * <pre>
     *     type="Any nullAllowed=true"
     * </pre>
     */
    private static StdArgList ANY_ARGLIST = new StdArgList(NO_FIXED, NO_OPTIONAL, new NamedArg[]{NAMED_NULLALLOWED});

    /**
     * Argument list for {@link #integerRangeType(Context, Object[])}. It accepts the named parameters that indicate
     * the range of integers.
     *
     * Example:
     * <pre>
     *     type="IngegerRange min=0 max=10"
     * </pre>
     */
    private static StdArgList INTEGERRANGE_ARGLIST = new StdArgList(NO_FIXED, NO_OPTIONAL, new NamedArg[]{NAMED_MIN, NAMED_MAX});

    /**
     * Argument list for {@link #oneOfType(Context, Object[])}. It accepts a variable number of type specs which
     * make up the union type. There must be at least two types for the union type to make sense and there is no
     * upper bound.
     *
     * Example:
     * <pre>
     *     type="OneOf (Integer) (Long) (BigDecimal)"
     * </pre>
     */
    private static VarArgList ONEOF_ARGLIST = new VarArgList(NO_FIXED, new VarArg(SPEC_TYPE), 2, -1, NO_NAMED);

    /**
     * Argument list for {@link #customSpec(Context, Object[])}. It accepts a class name.
     *
     * Example:
     * <pre>
     *     type="Custom branscha.MySpec"
     * </pre>
     */
    private static StdArgList CUSTOM_ARGLIST = new StdArgList(new FixedArg[]{FIXED_CLASSNAME}, NO_OPTIONAL, NO_NAMED);

    /**
     * Argument list for {@link #enumType(Context, Object[])}. It accepts a variable number of string labels which
     * are the values of the enum.
     *
     * Example:
     * <pre>
     *     type="Enum Uno Duo Tres"
     * </pre>
     */
    private static VarArgList ENUM_ARGLIST = new VarArgList(NO_FIXED, VAR_STRING, 1, -1, NO_NAMED);

    @ScriptyCommand(name = "BigDecimal")
    public static TypeSpec bigDecimalType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return BIGDECIMAL_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Integer")
    public static TypeSpec integerType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return INTEGER_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "BigInteger")
    public static TypeSpec bigIntegerType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return BIGINTEGER_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Boolean")
    public static TypeSpec booleanType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return BOOLEAN_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Byte")
    public static TypeSpec byteType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return BYTE_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "ListOf")
    public static TypeSpec listOfType(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = LISTOF_ARGLIST.guard(args, ctx);
            return new CheckedListType((TypeSpec) guarded[1], (Integer) guarded[2], (Integer) guarded[3]);
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Double")
    public static TypeSpec doubleType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return DOUBLE_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Float")
    public static TypeSpec floatType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return FLOAT_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "InstanceOrBinding")
    public static TypeSpec instanceOrBindingType(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = INSTANCEORBINDING_ARGLIST.guard(args, ctx);
            return new InstanceOrBinding((TypeSpec) guarded[1]);
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Instance")
    public static TypeSpec instanceType(Context ctx, Object[] args)
    throws CommandException {
        final int classNameIdx = 1;
        final int nullableIdx = 2;
        try {
            final Object[] guarded = INSTANCE_ARGLIST.guard(args, ctx);
            final String className = (String) guarded[classNameIdx];
            try {
                Class clazz = Class.forName(className);
                return new InstanceType(clazz, (Boolean) guarded[nullableIdx]);
            }
            catch (ClassNotFoundException e) {
                throw new CommandException(String.format("Could not find class '%s' for type Instance [CLASS].", className));
            }
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Any")
    public static TypeSpec anyType(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = ANY_ARGLIST.guard(args, ctx);
            return new AnyType((Boolean) guarded[1]);
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "IntegerRange")
    public static TypeSpec integerRangeType(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = INTEGERRANGE_ARGLIST.guard(args, ctx);
            return new IntegerRangeType((Integer) guarded[1], (Integer) guarded[2]);
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Long")
    public static TypeSpec longType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return LONG_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Short")
    public static TypeSpec shortType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return SHORT_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "String")
    public static TypeSpec stringType(Object[] args, Context ctx)
    throws CommandException {
        try {
            NOARGS_ARGLIST.guard(args, ctx);
            return STRING_TYPE;
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "OneOf")
    public static TypeSpec oneOfType(Context ctx, Object[] args)
    throws CommandException {
        try {
            ONEOF_ARGLIST.guard(args, ctx);
            // We do not need the cmd in the list.
            return new OrType(Arrays.copyOfRange(args, 1, args.length, TypeSpec[].class));
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Custom")
    public static TypeSpec customSpec(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = CUSTOM_ARGLIST.guard(args, ctx);
            final String className = (String) guarded[1];

            try {
                Class clazz = Class.forName(className);
                if (TypeSpec.class.isAssignableFrom(clazz)) {
                    return (TypeSpec) clazz.newInstance();
                }
                else {
                    throw new CommandException(String.format("The class '%s' does not seem to implement TypeSpec.", className));
                }
            }
            catch (ClassNotFoundException e) {
                String lMsg = String.format("Could not find class '%s'.", className);
                throw new CommandException(lMsg);
            }
            catch (InstantiationException | IllegalAccessException e) {
                String lMsg = String.format("Could not instantiate the custom typespec '%s'.", className);
                throw new CommandException(lMsg);
            }
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }

    @ScriptyCommand(name = "Enum")
    public static TypeSpec enumType(Context ctx, Object[] args)
    throws CommandException {
        try {
            final Object[] guarded = ENUM_ARGLIST.guard(args, ctx);

            List<String> enumLabels = new ArrayList<>(guarded.length - 1);
            for (int i = 1; i < args.length; i++) {
                enumLabels.add((String) guarded[i]);
            }
            return new EnumType(enumLabels);
        }
        catch (ArgSpecException e) {
            throw new CommandException(e.getMessage());
        }
    }
}
