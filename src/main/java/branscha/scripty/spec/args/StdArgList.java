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
import java.util.List;

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

    /**
     * An empty argument list for commands that do not accept any arguments.
     */
    public static final ArgList NOARG = new StdArgList(new FixedArg[]{}, new OptionalArg[]{}, new NamedArg[]{});

    private static final String ERR010 = "StdArgList/010: Badly formed argument list. It should at least contain the command name.";
    private static final String ERR020 = "StdArgList/020: Too many arguments. Expected at most %d arguments.";

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

        // Create a new argument list where we will collect the converted results.
        final Object[] newArgs = new Object[1 + fixedArgs.length + optArgs.length + namedArgs.length];
        int newArgsEnd = resolveFixedArgs(args, ctx, newArgs);

        // Check the optional ones.
        // Start looking after the fixed args, 
        // Provide the expected index.
        for (ArgSpec argSpec : optArgs) {
            newArgs[newArgsEnd] = argSpec.guard(args, newArgsEnd++, ctx);
        }

        // If there are still arguments left that are not pairs there are too many arguments.
        if (newArgsEnd < args.length && !(args[newArgsEnd] instanceof Pair))
            throw new ArgSpecException(String.format(ERR020, fixedArgs.length + optArgs.length));

        // Check the namedArgs args. We look for all pairs at the end of the argument list. We will only consider these
        // trailing pairs. We start at the back and work towards the front.
        int firstNamedArg = args.length - 1;
        while (firstNamedArg > 0 && (args[firstNamedArg] instanceof Pair)) firstNamedArg--;
        // Now we can resolve the namedArgs arguments within this range.
        resolveNamedArgs(args, ctx, newArgs, newArgsEnd, firstNamedArg);
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
}
