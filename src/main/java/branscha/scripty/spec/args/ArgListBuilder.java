/* ******************************************************************************
 * The MIT License
 * Copyright (c) 2012 Bruno Ranschaert
 * lib-scripty
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
package branscha.scripty.spec.args;

import branscha.scripty.ExtensionException;
import branscha.scripty.annot.ScriptyArg;
import branscha.scripty.annot.ScriptyStdArgList;
import branscha.scripty.annot.ScriptyVarArgList;
import branscha.scripty.repl.ReplEngine;
import branscha.scripty.repl.ReplEngineException;
import branscha.scripty.spec.map.ArgMapping;
import branscha.scripty.spec.map.ArrayIndexMapping;
import branscha.scripty.spec.map.PartialMapping;
import branscha.scripty.spec.type.TypeLanguageLib;
import branscha.scripty.spec.type.TypeSpec;

import java.util.HashMap;
import java.util.Map;

/**
 * A builder to convert the argument list definitions (in the form of annotations) to data structures that can be used
 * at runtime to do the type conversion and the type checking. The builder makes use of the {@link TypeLanguageLib} to
 * convert the type information in the annotations to {@link TypeSpec} information.
 */
public class ArgListBuilder {

    private static final String ERR010 = "ArgListBuilder/010: Internal Scripty error, could not initialize the argument type compiler.";
    private static final String ERR020 = "ArgListBuilder/020: Badly formed type expression '%s' encountered.%n%s";

    private static ReplEngine typeCompiler = new ReplEngine();
    static {
        try {
            typeCompiler.addLibraryClasses(TypeLanguageLib.class);
        }
        catch (ExtensionException e) {
            throw new Error(ERR010);
        }
    }

    /**
     * Compile a {@link StdArgList} annotation into a data structure that can be used to do the argument list conversions,
     * verifications and mapping when the associated command is invoked in Scripty. It translates the declarative
     * argument lists into a data structure that does the actual processing.
     *
     * @param stdArgListAnnotation The annotation that describes the argument list in a declarative way.
     * @return The compiled argument list information.
     */
    public static RuntimeArgList buildArgList(ScriptyStdArgList stdArgListAnnotation)
    throws ArgSpecException {

        /*
         * We collect the argument mappings  for the arguments we encounter. The key is the argument name, the value is
         * a mapper that can fetch the argument value and eventually we can inject that value in an annotated
         * method parameter. We must collect them so that we can provide what the parameter annotations ask for later on.
         * We create and collect the mappers here because we know the index where we will store the argument.
         */
        final Map<String, ArgMapping> argMappings = new HashMap<>();

        final ScriptyArg[] fixedArgAnnotations = stdArgListAnnotation.fixed();
        final FixedArg[] fixedArgs = new FixedArg[fixedArgAnnotations.length];

        final ScriptyArg[] optArgAnnotations = stdArgListAnnotation.optional();
        final OptionalArg[] optArgs = new OptionalArg[optArgAnnotations.length];

        final ScriptyArg[] namedArgAnnotations = stdArgListAnnotation.named();
        final NamedArg[] namedArgs = new NamedArg[namedArgAnnotations.length];

        // The index tracks the last argument spec in the argument list we are currently composing.
        int argIndex = 0;
        argIndex = compileFixedArgs(argMappings, fixedArgAnnotations, fixedArgs, argIndex);
        argIndex = compileOptArgs(argMappings, optArgAnnotations, optArgs, argIndex);
        compileNamedArgs(argMappings, namedArgAnnotations, namedArgs, argIndex);

        final ArgList stdArgList = new StdArgList(fixedArgs, optArgs, namedArgs);
        return new RuntimeArgList(stdArgList, argMappings);
    }

    /**
     * Compile a {@link VarArgList} annotation into a data structure that can be used to do the argument list conversions,
     * verifications and mapping when the associated command is invoked in Scripty. It translates the declarative
     * argument lists into a data structure that does the actual processing.
     *
     * @param varArgListAnnotation The annotation that describes the argument list in a declarative way.
     * @return The compiled argument list information.
     */
    public static RuntimeArgList buildArgList(ScriptyVarArgList varArgListAnnotation)
    throws ArgSpecException {

        /*
         * We collect the argument mappings  for the arguments we encounter. The key is the argument name, the value is
         * a mapper that can fetch the argument value and eventually we can inject that value in an annotated
         * method parameter. We must collect them so that we can provide what the parameter annotations ask for later on.
         * We create and collect the mappers here because we know the index where we will store the argument.
         */
        final Map<String, ArgMapping> argMappings = new HashMap<>();

        final ScriptyArg[] fixedAnnotations = varArgListAnnotation.fixed();
        final FixedArg[] fixedArgs = new FixedArg[fixedAnnotations.length];

        final ScriptyArg[] namedAnnotations = varArgListAnnotation.named();
        final NamedArg[] namedArgs = new NamedArg[namedAnnotations.length];

        final ScriptyArg varArgAnnotation = varArgListAnnotation.vararg();

        int argIdx = 0;
        argIdx = compileFixedArgs(argMappings, fixedAnnotations, fixedArgs, argIdx);
        argIdx = compileNamedArgs(argMappings, namedAnnotations, namedArgs, argIdx);
        VarArg varArg = compileVarArg(argMappings, varArgAnnotation, argIdx);

        final int varArgMinLength = varArgListAnnotation.minLength();
        final int varArgMaxLength = varArgListAnnotation.maxLength();
        final ArgList varArgList = new VarArgList(fixedArgs, varArg, varArgMinLength, varArgMaxLength, namedArgs);
        return new RuntimeArgList(varArgList, argMappings);
    }

    /**
     * Convert the variable argument annotation information {@link ScriptyArg} into the Java representation
     * of a variable argument {@link VarArg}. It compiles the annotation to its corresponding Java model.
     *
     * @param argMappings Accumulate mappings that we can use at runtime when mapping arguments to parameters.
     * @param varAnnotation The annotation we have to process.
     * @param argIndex The position of the next argument, relative to the complete argument list containing all argument
     *                 types: [cmd, [fixed args], [var arg], [named args]]
     * @return The resulting Java model.
     */
    private static VarArg compileVarArg(Map<String, ArgMapping> argMappings, ScriptyArg varAnnotation, int argIndex)
    throws ArgSpecException {
        final String argName = varAnnotation.name();
        final String argType = varAnnotation.type();
        try {
            final TypeSpec typeSpec = compileArgType(argType);
            final VarArg varArg = new VarArg(typeSpec);
            // Offset with 1, the mappings should skip element 0 which is the name of the command.
            // Note that we use a ParialMapping for var args. The var arg values can be injected
            // into an array parameter. We currently only support array parameters.
            argMappings.put(argName, new PartialMapping(argIndex + 1, -1));
            return varArg;
        }
        catch (ReplEngineException e) {
            throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
        }
    }

    /**
     * Convert the array of named argument annotations {@link ScriptyArg} into an array of the Java representation
     * of named arguments {@link NamedArg}. It compiles the annotations to a Java model.
     *
     * @param argMappings Accumulate mappings that we can use at runtime when mapping arguments to parameters.
     * @param namedAnnotations The annotations we have to process.
     * @param namedArgs The resulting Java model.
     * @param argIndex The position of the next argument, relative to the full argument list containing all argument
     *                 types: [cmd, [fixed args], [opt args], [named args]]
     * @return The updated free position in the global arg list.
     */
    private static int compileNamedArgs(Map<String, ArgMapping> argMappings, ScriptyArg[] namedAnnotations, NamedArg[] namedArgs, int argIndex)
    throws ArgSpecException {
        int namedIdx = 0;
        for (ScriptyArg namedAnnotation : namedAnnotations) {

            final String argName = namedAnnotation.name();
            final String argType = namedAnnotation.type();
            final boolean argIsOptional = namedAnnotation.optional();

            String argValue = namedAnnotation.value();
            if ("{null}".equals(argValue)) argValue = null;

            try {
                // Compile the type.
                final TypeSpec typeSpec = compileArgType(argType);
                namedArgs[namedIdx] = new NamedArg(argName, typeSpec, argValue, argIsOptional);
                // Offset with 1, the mappings should skip element 0 which is the name of the command.
                argMappings.put(argName, new ArrayIndexMapping(argIndex + 1));

                argIndex++;
                namedIdx++;
            }
            catch (ReplEngineException e) {
                throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
            }
        }
        return argIndex;
    }

    private static TypeSpec compileArgType(String argType)
    throws ReplEngineException {
        return (TypeSpec) typeCompiler.startNonInteractive(argType);
    }

    /**
     * Convert the array of fixed argument annotations {@link ScriptyArg} into an array of the Java representation
     * of fixed arguments {@link FixedArg}. It compiles the annotations to a Java model.
     *
     * @param argMappings Accumulate mappings that we can use at runtime when mapping arguments to parameters.
     * @param fixedArgAnnotations The annotations we have to process.
     * @param fixedArgs The resulting Java model.
     * @param argIndex The position of the next argument, relative to the full argument list containing all argument
     *                 types: [cmd, [fixed args], [opt args], [named args]]
     * @return The updated free position in the global arg list.
     */
    private static int compileFixedArgs(Map<String, ArgMapping> argMappings, ScriptyArg[] fixedArgAnnotations, FixedArg[] fixedArgs, int argIndex)
    throws ArgSpecException {

        for (ScriptyArg argAnnotation : fixedArgAnnotations) {
            final String argName = argAnnotation.name();
            final String argType = argAnnotation.type();
            // Default value is ignored for fixed args.
            // Optional flag is ignored for fixed args.

            try {
                final TypeSpec typeSpec = compileArgType(argType);
                fixedArgs[argIndex] = new FixedArg(typeSpec);
                // Offset with 1, the mappings should skip element 0 which is the name of the command.
                argMappings.put(argName, new ArrayIndexMapping(argIndex + 1));
                argIndex++;
            }
            catch (ReplEngineException e) {
                throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
            }
        }
        return argIndex;
    }

    /**
     * Convert the array of optional argument annotations {@link ScriptyArg} into an array of the Java representation
     * of optional arguments {@link OptionalArg}. It compiles the annotations to a Java model.
     *
     * @param argMappings Accumulate named mappings that we can use at runtime when mapping arguments to parameters.
     * @param optArgAnnotations The annotations we have to process.
     * @param optArgs The resulting Java model.
     * @param argIndex The position of the next argument, relative to the full argument list containing all argument
     *                 types: [cmd, [fixed args], [opt args], [named args]]
     * @return The updated free position in the global arg list.
     */
    private static int compileOptArgs(Map<String, ArgMapping> argMappings, ScriptyArg[] optArgAnnotations, OptionalArg[] optArgs, int argIndex)
    throws ArgSpecException {
        int optIdx = 0;

        for (ScriptyArg optArgAnnotation : optArgAnnotations) {

            final String argName = optArgAnnotation.name();
            final String argType = optArgAnnotation.type();
            String argValue = optArgAnnotation.value();
            // Optional flag is not needed, optional args are always optional.

            if ("{null}".equals(argValue)) argValue = null;

            try {
                final TypeSpec typeSpec = compileArgType(argType);
                optArgs[optIdx] = new OptionalArg(typeSpec, argValue);
                // Offset with 1, the mappings should skip element 0 which is the name of the command.
                argMappings.put(argName, new ArrayIndexMapping(argIndex + 1));

                argIndex++;
                optIdx++;
            }
            catch (ReplEngineException e) {
                throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
            }
        }
        return argIndex;
    }
}