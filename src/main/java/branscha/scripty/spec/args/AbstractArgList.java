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

public abstract class AbstractArgList implements ArgList {

    private static final String ERR010 = "AbstractArgList/010: Found an badly formed named argument, where the name is not a string but an instance of type '%s'.";
    private static final String ERR020 = "AbstractArgList/020: Found an unexpected named argument '%s'.";
    private static final String ERR030 = "AbstractArgList/030: Not enough arguments. Expected at least %d arguments and received %d.";

    protected FixedArg[] fixedArgs;
    protected NamedArg[] namedArgs;

    public AbstractArgList(FixedArg fixedArgs[], NamedArg[] namedArgs) {
        this.fixedArgs = fixedArgs;
        this.namedArgs = namedArgs;
    }

    int resolveNamedArgs(Object[] args, Context ctx, Object[] newArgs, int newArgsEnd, int firstNamedArg)
    throws ArgSpecException {

        for (ArgSpec argSpec : namedArgs) {
            newArgs[newArgsEnd++] = argSpec.guard(args, firstNamedArg, ctx);
        }

        // Finally we go looking for spurious named arg parameters that were not specified ...
        for (int i = firstNamedArg; i < args.length; i++) {
            if (args[i] instanceof Pair) {
                final Pair pair = (Pair) args[i];
                if (!(pair.getLeft() instanceof String))
                    throw new ArgSpecException(String.format(ERR010, pair.getLeft() == null ? "null" : pair.getLeft().getClass().getCanonicalName()));
                String pairName = (String) pair.getLeft();
                boolean found = false;
                for (NamedArg namedArg : namedArgs) {
                    if (namedArg.getName().equals(pairName)) {
                        found = true;
                        break;
                    }
                }
                if (!found)
                    throw new ArgSpecException(String.format(ERR020, pairName));
            }
        }
        return newArgsEnd;
    }

    int resolveFixedArgs(Object[] args, Context ctx, Object[] newArgs)
    throws ArgSpecException {
        // Copy the command name to the new argument list, the structure will remain the same.
        newArgs[0] = args[0];

        // Check the fixed.
        if (args.length - 1 < fixedArgs.length)
            throw new ArgSpecException(String.format(ERR030, fixedArgs.length, args.length - 1));

        // We skip the command name.
        int newArgsEnd = 1;
        for (FixedArg fixedArg : fixedArgs) {
            newArgs[newArgsEnd] = fixedArg.guard(args, newArgsEnd++, ctx);
        }
        return newArgsEnd;
    }
}
