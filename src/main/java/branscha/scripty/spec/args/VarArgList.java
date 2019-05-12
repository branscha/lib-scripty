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

/**
 * There are 3 types of parameters in a variable argument list.
 *
 * <ol>
 * <li>Fixed (always required) arguments, each argument has its own type.</li>
 * <li>Variable length. These all have the same type. These cannot be pairs.
 * It is the same type that can occur min to max times. A repetition of the same type. Eg. a list of 1 or more integers.</li>
 * <li>Named (optional or required). These are pairs at the end of the command line. The namedArgs parameters can have a default value.</li>
 * </ol>
 * <p>
 * Warning: The fixed args will be put first in the argument list.
 * After that, the (!) namedArgs args will be put in the list.
 * Finally, the variable part.
 */
public class VarArgList extends AbstractArgList {

    private static final String ERR010 = "VarArgList/010: Badly formed argument list. It should at least start with the command name.";
    private static final String ERR020 = "VarArgList/020: Too few arguments of type '%s'. Expected at least %d and received %d.";
    private static final String ERR030 = "VarArgList/030: Too many arguments of type '%s'. Expected at most %d and received %d.";

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
            throw new ArgSpecException(ERR010);
        }

        // We look for all pairs at the end of the argument list. We will only
        // consider these trailing pairs.
        int firstNamedArg = args.length - 1;
        while (firstNamedArg > 0 && (args[firstNamedArg] instanceof Pair)) firstNamedArg--;
        int nrVarArgs = firstNamedArg - fixedArgs.length;
        if (nrVarArgs < 0) nrVarArgs = 0;

        if (min >= 0 && nrVarArgs < min)
            throw new ArgSpecException(String.format(ERR020, varArg.getSpecName(), min, nrVarArgs));
        if (max >= 0 && nrVarArgs > max)
            throw new ArgSpecException(String.format(ERR030, varArg.getSpecName(), max, nrVarArgs));

        // Create a new argument list where we will accumulate the converted results.
        Object[] newArgs = new Object[1 + fixedArgs.length + nrVarArgs + namedArgs.length];

        // Copy the command name to the new argument list, the structure will remain the same.
        newArgs[0] = args[0];
        int newArgsEnd = 1;

        newArgsEnd = resolveFixedArgs(args, ctx, newArgs, newArgsEnd);

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

    public static class Builder {
        private List<FixedArg> fixed = new ArrayList<>();
        private List<NamedArg> named = new ArrayList<>();
        private VarArg varArg;
        private int minOccurs=0, maxOccurs=-1;

        public VarArgList build() {
            return new VarArgList(
                    fixed.toArray(new FixedArg[0]),
                    varArg, minOccurs, maxOccurs,
                    named.toArray(new NamedArg[0]));
        }

        public VarArgList.Builder addFixed(FixedArg arg) {
            fixed.add(arg);
            return this;
        }

        public VarArgList.Builder addNamed(NamedArg arg) {
            named.add(arg);
            return this;
        }

        public VarArgList.Builder addVarArg(VarArg arg, int minOccurs, int maxOccurs) {
            this.varArg = arg;
            this.minOccurs = minOccurs;
            this.maxOccurs = maxOccurs;
            return this;
        }
    }

    @Override
    public String toString() {
        final String indent = "   ";
        final StringBuilder sb = new StringBuilder("VarArgList{");
        if(fixedArgs != null && fixedArgs.length > 0){
            sb.append("\n").append(indent).append("fixed=").append(Arrays.asList(fixedArgs).toString());
        }
        sb.append("\n").append(indent).append("var=").append(varArg);
        if(min > 0) {
            sb.append(", #min=").append(min);
        }
        if(max > 0) {
            sb.append(", #max=").append(max);
        }
        if(namedArgs != null && namedArgs.length > 0) {
            sb.append("\n").append(indent).append("named=").append(namedArgs == null ? "null" : Arrays.asList(namedArgs).toString());
        }
        sb.append('}');
        return sb.toString();
    }
}
