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
 * The named parameters can have a default value.
 * <p>
 * Warning: The fixed args will be put first in the argument list.
 * After that, the (!) named args will be put in the list.
 * Finally, the variable part.
 */
public class VarArgList implements ArgList {

    private FixedArg req[];
    private VarArg var;
    private NamedArg[] named;
    int min, max;

    public VarArgList(FixedArg aReq[], VarArg aVar, int aMin, int aMax, NamedArg[] aNamed) {
        req = aReq;
        var = aVar;
        named = aNamed;
        min = aMin;
        max = aMax;
    }

    /**
     * The args are expected to have the form ("CMD", arg-1, arg-2, ... arg-n).
     * The result is in this order:
     * - First the fixed.
     * - Then !!! the named.
     * - Lastly the varargs.
     */
    public Object[] guard(Object[] aArgs, Context aCtx)
    throws ArgSpecException {
        // We look for all pairs at the end of the argument list. We will only
        // consider these trailing pairs.
        int lStartNamed = aArgs.length - 1;
        while (lStartNamed > 0 && (aArgs[lStartNamed] instanceof Pair)) lStartNamed--;
        int lNrVar = lStartNamed - req.length;
        if (lNrVar < 0) lNrVar = 0;

        if (min >= 0 && lNrVar < min)
            throw new ArgSpecException(String.format("Too few arguments of type '%s'. Expected at least %d and received %d.", var.getSpecName(), min, lNrVar));
        if (max >= 0 && lNrVar > max)
            throw new ArgSpecException(String.format("Too many arguments of type '%s'. Expected at most %d and received %d.", var.getSpecName(), max, lNrVar));

        // Create a new argument list where we will accumulate the
        // converted results.
        Object[] lNewArgs = new Object[1 + req.length + lNrVar + named.length];
        // Copy the command name to the new argument list, the structure will remain the same.
        lNewArgs[0] = aArgs[0];

        // Check the fixed.
        if (aArgs.length - 1 < req.length)
            throw new ArgSpecException(String.format("Too few arguments. Expected at least %d arguments but received %d.", req.length, aArgs.length - 1));

        // We skip the command name.
        int lArgIdx = 1;
        for (int i = 0; i < req.length; i++) {
            lNewArgs[lArgIdx] = req[i].guard(aArgs, lArgIdx++, aCtx);
        }

        // Check the named args.
        // We look for all pairs at the end of the argument list. We will only
        // consider these trailing pairs.
        while (lStartNamed > 0 && (aArgs[lStartNamed] instanceof Pair)) lStartNamed--;
        // Now we can resolve the named arguments within this range.
        for (ArgSpec lSpec : named) {
            lNewArgs[lArgIdx++] = lSpec.guard(aArgs, lStartNamed, aCtx);
        }
        // Finally we go looking for spurious named parameters that were not specified ...
        for (int i = lStartNamed; i < aArgs.length; i++) {
            if (aArgs[i] instanceof Pair) {
                final Pair lPair = (Pair) aArgs[i];
                if (!(lPair.getLeft() instanceof String))
                    throw new ArgSpecException(String.format("Found an badly formed named argument, where the name is not a string but an instance of type '%s'.", lPair.getLeft() == null ? "null" : lPair.getLeft().getClass().getCanonicalName()));
                String lPairName = (String) lPair.getLeft();
                boolean found = false;
                for (int j = 0; j < named.length; j++) {
                    if (named[j].getName().equals(lPairName)) {
                        found = true;
                        break;
                    }
                }
                if (!found)
                    throw new ArgSpecException(String.format("Found an unexpected named argument '%s'.", lPairName));
            }
        }

        // Check the var ones.
        // Start looking after the fixed args,
        // Provide the expected index.
        // Skip the cmd in the beginning of the arguments.
        final int lStartVar = 1 + req.length;
        for (int i = 0; i < lNrVar; i++) {
            lNewArgs[lArgIdx++] = var.guard(aArgs, lStartVar + i, aCtx);
        }
        return lNewArgs;
    }
}
