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

/**
 * There are 3 types of parameters in a variable argument list.
 * 1. Fixed (always required) arguments, each argument has its own type.
 * 2. Variable length. These all have the same type. These cannot be pairs.
 * It is the same type that can occur min to max times. A repetition of the same type.
 * Eg. a list of 1 or more integers.
 * 3. Named (optional or required). These are pairs at the end of the command line.
 * The namedArgs parameters can have a default value.
 * <p>
 * Warning: The fixed args will be put first in the argument list.
 * After that, the (!) namedArgs args will be put in the list.
 * Finally, the variable part.
 */
public class VarArgList extends AbstractArgList {

    private VarArg varArg;
    private int min, max;

    public VarArgList(FixedArg[] fixedArgs, VarArg varArg, int minOccurs, int maxOccurs, NamedArg[] namedArgs) {

        super(fixedArgs, namedArgs);
        this.varArg = varArg;
        min = minOccurs;
        max = maxOccurs;
    }

    /**
     * The args are expected to have the form ("CMD", arg-1, arg-2, ... arg-n).
     * The result is in this order:
     * - First the fixed.
     * - Then !!! the namedArgs.
     * - Lastly the varargs.
     */
    public Object[] guard(Object[] args, Context ctx)
    throws ArgSpecException {
        if(args == null || args.length < 1) {
            throw new ArgSpecException("Badly formed argument list. It should ad least start with the command name.");
        }

        // We look for all pairs at the end of the argument list. We will only
        // consider these trailing pairs.
        int firstNamedArg = args.length - 1;
        while (firstNamedArg > 0 && (args[firstNamedArg] instanceof Pair)) firstNamedArg--;
        int nrVarArgs = firstNamedArg - fixedArgs.length;
        if (nrVarArgs < 0) nrVarArgs = 0;

        if (min >= 0 && nrVarArgs < min)
            throw new ArgSpecException(String.format("Too few arguments of type '%s'. Expected at least %d and received %d.", varArg.getSpecName(), min, nrVarArgs));
        if (max >= 0 && nrVarArgs > max)
            throw new ArgSpecException(String.format("Too many arguments of type '%s'. Expected at most %d and received %d.", varArg.getSpecName(), max, nrVarArgs));

        // Create a new argument list where we will accumulate the converted results.
        Object[] newArgs = new Object[1 + fixedArgs.length + nrVarArgs + namedArgs.length];
        // Copy the command name to the new argument list, the structure will remain the same.
        int newArgsEnd = resolveFixedArgs(args, ctx, newArgs);

        // Check the namedArgs args.
        // We look for all pairs at the end of the argument list. We will only consider these trailing pairs.
        while (firstNamedArg > 0 && (args[firstNamedArg] instanceof Pair)) firstNamedArg--;
        // Now we can resolve the namedArgs arguments within this range.
        newArgsEnd = resolveNamedArgs(args, ctx, newArgs, newArgsEnd, firstNamedArg);

        // Check the varArg ones.
        // Start looking after the fixed args, Provide the expected index. Skip the cmd in the beginning of the arguments.
        final int firstVarArg = 1 + fixedArgs.length;
        for (int i = 0; i < nrVarArgs; i++) {
            newArgs[newArgsEnd++] = varArg.guard(args, firstVarArg + i, ctx);
        }
        return newArgs;
    }
}
