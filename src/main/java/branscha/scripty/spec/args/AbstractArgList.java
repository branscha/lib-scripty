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

import branscha.scripty.parser.Context;
import branscha.scripty.parser.Pair;
import branscha.scripty.spec.type.TypeSpecException;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

abstract class AbstractArgList implements ArgList {

    private static final String ERR010 = "AbstractArgList/010: Found a badly formed named argument, where the name is not a string but an instance of type '%s'.";
    private static final String ERR020 = "AbstractArgList/020: Found an unexpected named argument '%s'.";
    private static final String ERR030 = "AbstractArgList/030: Not enough arguments. Expected at least %d arguments and received %d.";
    private static final String ERR040 = "AbstractArgList/040: Named argument(s) '%s' missing.";
    private static final String ERR050 = "AbstractArgList/050: Error while converting a default value of a named parameter.%n%s";

    /* The pattern that a named arg name can have. It cannot start with a digit */
    private static final Pattern NAME_PATTERN = Pattern.compile("--?(\\D\\w*)");

    FixedArg[] fixedArgs;
    NamedArg[] namedArgs;

    AbstractArgList(FixedArg[] fixedArgs, NamedArg[] namedArgs) {
        this.fixedArgs = fixedArgs;
        this.namedArgs = namedArgs;
    }

    int resolveNamedArgs(final Object[] args, final Context ctx, final Object[] newArgs, final int newArgsFreePos,
                         final int firstNamedArg, final int lastNamedArg)
    throws ArgSpecException {

        // First make an inventory of the named arguments (both required and optional).
        // * The required list is used to verify that we received all required arguments.
        // * The optional list is used to calculate the default values for missing arguments.
        final Set<Integer> requiredArgs = new HashSet<>();
        final Set<Integer> optionalArgs = new HashSet<>();
        for (int namedIdx = 0; namedIdx < namedArgs.length; namedIdx++) {
            final NamedArg argSpec = namedArgs[namedIdx];
            if (argSpec.isOptional()) {
                optionalArgs.add(namedIdx);
            }
            else {
                requiredArgs.add(namedIdx);
            }
        }

        // Visit all named arguments in the command line and handle these.
        // Also note which named arguments we have encountered.
        for (int argIdx = firstNamedArg; argIdx >= 0 && argIdx <= lastNamedArg; argIdx++) {
            if (args[argIdx] instanceof String) {

                // 1. String named variables never contain a second argument.
                //    They always have a boolean nature.

                final String flags = (String) args[argIdx];
                Optional<Integer> namedIdx = Optional.empty();
                if (flags.startsWith("--")) {
                    final String name = flags.substring(2);
                    namedIdx = findNamedArgByName(name);
                    handleNamedArg(name, namedIdx, Boolean.TRUE, ctx, newArgs, newArgsFreePos, requiredArgs, optionalArgs);
                }
                else if (flags.startsWith("-")) {
                    for (char flag : flags.substring(1).toCharArray()) {
                        namedIdx = findNamedArgByFlag(String.valueOf(flag));
                        handleNamedArg("flag: '" + flag + "'", namedIdx, Boolean.TRUE, ctx, newArgs, newArgsFreePos, requiredArgs, optionalArgs);
                    }
                }
                else {
                    // Cannot occur.
                    throw new ArgSpecException("Named var should start with - or --");
                }
            }
            else if (args[argIdx] instanceof Pair) {

                // 2. Pair named variables always have a right hand side.
                //    Long names have a single left side and right side.
                //    Short flags can have multiple boolean flags, the last flag gets the right hand side.

                final Pair pair = (Pair) args[argIdx];
                if (!(pair.getLeft() instanceof String)) {
                    throw new ArgSpecException(String.format(ERR010,
                            pair.getLeft() == null ? "null" : pair.getLeft().getClass().getCanonicalName()));
                }
                String pairName = (String) pair.getLeft();
                Object pairValue = pair.getRight();

                if(pairName.startsWith("--")) {
                    // Cut off dashes.
                    pairName = pairName.substring(2);
                    final Optional<Integer> namedIdx = findNamedArgByName(pairName);
                    handleNamedArg(pairName, namedIdx, pairValue, ctx, newArgs, newArgsFreePos, requiredArgs, optionalArgs);
                }
                else if(pairName.startsWith("-")) {
                    final char[] flagArray = pairName.substring(1).toCharArray();
                    for (int i = 0; i < flagArray.length; i++) {
                        char flag = flagArray[i];
                        final Optional<Integer> namedIdx = findNamedArgByFlag(String.valueOf(flag));
                        handleNamedArg("flag: '" + flag + "'",namedIdx, i == (flagArray.length -1) ? pairValue : Boolean.TRUE, ctx, newArgs, newArgsFreePos, requiredArgs, optionalArgs);
                    }
                }
                else {
                    // Cannot occur.
                    throw new ArgSpecException("Named var should start with - or --");
                }


            }
        }

        // Verify that all required arguments were present in the command line.
        // That is why the 'requiredArgs'list was necessary in the first place.
        if(requiredArgs.size() > 0) {
            Set<String> names = requiredArgs.stream()
                    .map(i->namedArgs[i].getName())
                    .collect(Collectors.toSet());
            throw new ArgSpecException(String.format(ERR040, names.toString()));
        }

        // Get the default values for the optional named arguments which we did encounter in the command line.
        // That is the purpose of the 'optionalArgs' list.
        try {
            for(Integer namedIdx: optionalArgs) {
                newArgs[newArgsFreePos + namedIdx] = namedArgs[namedIdx].getDefaultVal(ctx);
            }
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format(ERR050, e.getMessage()));
        }

        return newArgsFreePos + namedArgs.length;
    }

    private void handleNamedArg(String name, Optional<Integer> namedIdx, Object arg, Context ctx, Object[] newArgs, int newArgsFreePos, Set<Integer> requiredArgs, Set<Integer> optionalArgs)
    throws ArgSpecException {

        if (namedIdx.isPresent()) {
            int idx = namedIdx.get();
            newArgs[newArgsFreePos + idx] = namedArgs[idx].guard(new Object[]{arg}, 0, ctx);
            // Keep track of the named arguments we have seen.
            requiredArgs.remove(idx);
            optionalArgs.remove(idx);
        }
        else {
            throw new ArgSpecException(String.format(ERR020, name));
        }
    }

    private Optional<Integer> findNamedArgByName(final String argName) {
        for (int namedIdx = 0; namedIdx < namedArgs.length; namedIdx++) {
            if (namedArgs[namedIdx].getName().equals(argName)) {
                return Optional.of(namedIdx);
            }
        }
        return Optional.empty();
    }

    private Optional<Integer> findNamedArgByFlag(final String flag) {
        for (int namedIdx = 0; namedIdx < namedArgs.length; namedIdx++) {
            if (namedArgs[namedIdx].getFlag().equals(flag)) {
                return Optional.of(namedIdx);
            }
        }
        return Optional.empty();
    }

    int resolveFixedArgs(Object[] args, final int argsOffset, Context ctx, Object[] newArgs, int newArgsFreePos)
    throws ArgSpecException {
        // Check the fixed.
        if (args.length - argsOffset - 1 < fixedArgs.length) {
            throw new ArgSpecException(String.format(ERR030, fixedArgs.length, args.length - argsOffset - 1));
        }

        for (FixedArg fixedArg : fixedArgs) {
            newArgs[newArgsFreePos] = fixedArg.guard(args, argsOffset + newArgsFreePos++, ctx);
        }

        return newArgsFreePos;
    }

    int findLastNamedArg(Object[] args, int firstNamedArg) {
        if (firstNamedArg > 0) {
            // arg[0] is cmd name.
            for (int i = firstNamedArg + 1; i < args.length; i++) {
                if (!isNamedArg(args[i])) {
                    return i - 1;
                }
            }
            return args.length - 1;
        }
        return firstNamedArg - 1;
    }

    Optional<Integer> findFirstNamedArgAtStart(Object[] args, int offset) {
        for (int i = offset; i < args.length; i++) {
            if (isNamedArg(args[i])) return Optional.of(i);
        }
        return Optional.empty();
    }

    private boolean isNamedArg(Object arg) {
        if (arg instanceof String && ((String) arg).startsWith("-")) {
            // Eg -i -l --list
            return (NAME_PATTERN.matcher((String) arg).matches());
        }
        else {
            // Eg -f=FILE --file=file -xzf=FILE
            return arg instanceof Pair && ((Pair) arg).getLeft() instanceof String && ((String) ((Pair) arg).getLeft()).startsWith("-");
        }
    }
}
