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

    private static ReplEngine typeProcessor = new ReplEngine();

    private static final String ERR010 = "ArgListBuilder/010: Internal Scripty error, could not initialize the internal type system.";
    private static final String ERR020 = "ArgListBuilder/020: Badly formed type expression '%s' encountered.%n%s";

    static {
        try {
            typeProcessor.addLibraryClasses(TypeLanguageLib.class);
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
        // We will collect the mappings we encounter in this map.
        Map<String, ArgMapping> argMappings = new HashMap<>();

        ScriptyArg[] fixedArgAnnotations = stdArgListAnnotation.fixed();
        FixedArg[] fixedArgSpecs = new FixedArg[fixedArgAnnotations.length];

        ScriptyArg[] optArgAnnotations = stdArgListAnnotation.optional();
        OptionalArg[] optArgSpecs = new OptionalArg[optArgAnnotations.length];

        ScriptyArg[] namedArgAnnotations = stdArgListAnnotation.named();
        NamedArg[] namedArgSpecs = new NamedArg[namedArgAnnotations.length];

        // The index tracks the last argument spec in the argument list we are currently composing.
        int argIndex = compileFixedArgs(argMappings, fixedArgAnnotations, fixedArgSpecs);

        int j = 0;

        for (ScriptyArg optArgAnnotation : optArgAnnotations) {

            String argName = optArgAnnotation.name();
            String argType = optArgAnnotation.type();
            String argValue = optArgAnnotation.value();
            // Optional flag is not needed, optional args are always optional.

            if ("{null}".equals(argValue)) argValue = null;

            try {

                TypeSpec lTypeSpec = (TypeSpec) typeProcessor.startNonInteractive(argType);
                optArgSpecs[j] = new OptionalArg(lTypeSpec, argValue);
                // Offset with 1, the mappings should skip element 0 which is the name of the command.
                argMappings.put(argName, new ArrayIndexMapping(argIndex + 1));

                argIndex++;
                j++;
            }
            catch (ReplEngineException e) {
                throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
            }
        }

        compileNamedArgs(argMappings, namedArgAnnotations, namedArgSpecs, argIndex);

        ArgList lStdArgList = new StdArgList(fixedArgSpecs, optArgSpecs, namedArgSpecs);
        return new RuntimeArgList(lStdArgList, argMappings);
    }

    /**
     * Compile a {@link VarArgList} annotation into a data structure that can be used to do the argument list conversions,
     * verifications and mapping when the associated command is invoked in Scripty. It translates the declarative
     * argument lists into a data structure that does the actual processing.
     *
     * @param varArgList The annotation that describes the argument list in a declarative way.
     * @return The compiled argument list information.
     */
    public static RuntimeArgList buildArgList(ScriptyVarArgList varArgList)
    throws ArgSpecException {
        // We will collect the mappings we encounter in this argument list.
        // If we encounter a named argument, we will remember the piece of code that fetches
        // that argument from the guarded argument list.
        Map<String, ArgMapping> argMappings = new HashMap<>();

        ScriptyArg[] lFixedArgs = varArgList.fixed();
        FixedArg[] lFixedSpecs = new FixedArg[lFixedArgs.length];

        ScriptyArg[] lNamedArgs = varArgList.named();
        NamedArg[] lNamedSpecs = new NamedArg[lNamedArgs.length];

        ScriptyArg varArg = varArgList.vararg();
        VarArg varArgSpec;
        int varArgMinLength = varArgList.minLength();
        int varArgMaxLength = varArgList.maxLength();

        int argIdx = compileFixedArgs(argMappings, lFixedArgs, lFixedSpecs);
        argIdx = compileNamedArgs(argMappings, lNamedArgs, lNamedSpecs, argIdx);

        String argName = varArg.name();
        String argType = varArg.type();

        try {
            TypeSpec typeSpec = (TypeSpec) typeProcessor.startNonInteractive(argType);
            varArgSpec = new VarArg(typeSpec);
            // Offset with 1, the mappings should skip element 0 which is the name of the command.
            // Note that we use a ParialMapping for var args. The var arg values can be injected
            // into an array parameter. We currently only support array parameters.
            argMappings.put(argName, new PartialMapping(argIdx + 1, -1));
        }
        catch (ReplEngineException e) {
            throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
        }

        ArgList argList = new VarArgList(lFixedSpecs, varArgSpec, varArgMinLength, varArgMaxLength, lNamedSpecs);
        return new RuntimeArgList(argList, argMappings);
    }

    private static int compileNamedArgs(Map<String, ArgMapping> argMappings, ScriptyArg[] lNamedArgs, NamedArg[] lNamedSpecs, int argIndex)
    throws ArgSpecException {
        int k = 0;

        for (ScriptyArg lArg : lNamedArgs) {
            String argName = lArg.name();
            String argType = lArg.type();
            String argValue = lArg.value();
            boolean argIsOptional = lArg.optional();

            if ("{null}".equals(argValue)) argValue = null;

            try {
                TypeSpec typeSpec = (TypeSpec) typeProcessor.startNonInteractive(argType);
                lNamedSpecs[k] = new NamedArg(argName, typeSpec, argValue, argIsOptional);
                // Offset with 1, the mappings should skip element 0 which is the name of the command.
                argMappings.put(argName, new ArrayIndexMapping(argIndex + 1));

                argIndex++;
                k++;
            }
            catch (ReplEngineException e) {
                throw new ArgSpecException(String.format(ERR020, argType, e.getMessage()));
            }
        }
        return argIndex;
    }

    private static int compileFixedArgs(Map<String, ArgMapping> argMappings, ScriptyArg[] fixedArgAnnotations, FixedArg[] fixedArgSpecs)
    throws ArgSpecException {
        int argIndex = 0;

        for (ScriptyArg argAnnotation : fixedArgAnnotations) {
            String argName = argAnnotation.name();
            String argType = argAnnotation.type();
            // Default value is ignored for fixed args.
            // Optional flag is ignored for fixed args.

            try {
                TypeSpec typeSpec = (TypeSpec) typeProcessor.startNonInteractive(argType);
                fixedArgSpecs[argIndex] = new FixedArg(typeSpec);
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
}