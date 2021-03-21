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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * There are 3 types of parameters in a standard argument list.
 *
 * <ol>
 * <li>Fixed, required arguments. Each with its own type.</li>
 * <li>Optional. Each argument has its own type. These cannot be of type Pair, because this might conflict with the
 * namedArgs arguments. The optional parameters can have a default value which will be used when the argument is not
 * present.</li>
 * <li>Named (optional or required). These are pairs at the end of the command line. The namedArgs parameters can have
 * a default value.</li>
 * </ol>
 *
 * Also see {@link ArgList}
 */
public class StdArgList extends AbstractArgList {

    private static final String ERR010 = "StdArgList/010: Badly formed argument list. It should at least contain the command name.";
    private static final String ERR020 = "StdArgList/020: Too many arguments. Expected at most %d arguments.";
    private static final String ERR030 = "StdArgList/030: Found named argument '%s' at a wrong location, named arguments should be at the beginning.";

    private OptionalArg[] optArgs;

    public StdArgList(FixedArg[] fixedArgs, OptionalArg[] optionalArgs, NamedArg[] namedArgs) {
        super(fixedArgs, namedArgs);
        optArgs = optionalArgs;
    }

    /**
     * See {@link ArgList#guard(Object[], Context)}.
     * The arguments are expected to have the form ("CMD", arg-1, arg-2, ... arg-n).
     */
    public Object[] guard(Object[] args, Context ctx)
    throws ArgSpecException {

        if(args == null || args.length < 1) {
            throw new ArgSpecException(ERR010);
        }

        // Create a new argument list to collect the converted results.
        final Object[] newArgs = new Object[1 + fixedArgs.length + optArgs.length + namedArgs.length];

        // Copy the command name to the new argument list, the structure will remain the same.
        newArgs[0] = args[0];
        int newArgsFreePos = 1;
        int nrNamedArgs = 0;
        int lastNamedArg = -2;

        // Check the namedArgs args. We look for all pairs at the end of the argument list. We will only consider these
        // trailing pairs. We start at the back and work towards the front.
        final Optional<Integer> firstNamedArg = findFirstNamedArgAtStart(args, 1);
        if(firstNamedArg.isPresent()) {
            // If there is a first one, there must be a last one as well.
            lastNamedArg = findLastNamedArg(args, firstNamedArg.get());
            nrNamedArgs = lastNamedArg - firstNamedArg.get() + 1;

            final Optional<Integer> rogue = findFirstNamedArgAtStart(args, lastNamedArg + 1);
            if(rogue.isPresent()) {
                throw new ArgSpecException(String.format(ERR030, args[rogue.get()].toString()));
            }
        }

        // 1. FIXED
        newArgsFreePos = resolveFixedArgs(args, nrNamedArgs, ctx, newArgs, newArgsFreePos);

        // 2. OPTIONAL
        for (ArgSpec argSpec : optArgs) {
            newArgs[newArgsFreePos] = argSpec.guard(args, nrNamedArgs + newArgsFreePos++, ctx);
        }

        // 3. NAMED
        newArgsFreePos = resolveNamedArgs(args, ctx, newArgs, newArgsFreePos, firstNamedArg.orElse(-1), lastNamedArg);

        // If there are still arguments left there are too many arguments.
        if (newArgsFreePos < args.length) {
            throw new ArgSpecException(String.format(ERR020, fixedArgs.length + optArgs.length));
        }

        return newArgs;
    }

    public static class Builder {
        private List<FixedArg> fixed = new ArrayList<>();
        private List<NamedArg> named = new ArrayList<>();
        private List<OptionalArg> optional = new ArrayList<>();

        public StdArgList build() {
            return new StdArgList(
                    fixed.toArray(new FixedArg[0]),
                    optional.toArray(new OptionalArg[0]),
                    named.toArray(new NamedArg[0]));
        }

        public Builder addFixed(FixedArg arg) {
            fixed.add(arg);
            return this;
        }

        public Builder addNamed(NamedArg arg) {
            named.add(arg);
            return this;
        }

        public Builder addOptional(OptionalArg arg) {
            optional.add(arg);
            return this;
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("StdArgList{");
        if (fixedArgs != null && fixedArgs.length > 0) {
            sb.append("\n").append("   ").append("fixed=").append(Arrays.asList(fixedArgs).toString());
        }
        if (optArgs != null && optArgs.length > 0) {
            sb.append("\n").append("   ").append("optional=").append(Arrays.asList(optArgs).toString());
        }
        if (namedArgs != null && namedArgs.length > 0) {
            sb.append("\n").append("   ").append("named=").append(Arrays.asList(namedArgs).toString());
        }
        sb.append('}');
        return sb.toString();
    }
}
