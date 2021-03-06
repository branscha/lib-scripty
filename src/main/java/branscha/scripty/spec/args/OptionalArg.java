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
import branscha.scripty.spec.type.TypeSpec;
import branscha.scripty.spec.type.TypeSpecException;

/**
 * Optional args are positional (have a fixed location). An optional parameter
 * can be omitted, in which case the default value will be used.
 * <p>
 * It is assumed that fixed args come first, then optional ones and
 * eventually named parameters.
 * <p>
 * An optional parameter cannot be a pair, because it could conflict
 * with the named parameters which are grouped at the end of the list.
 */
public class OptionalArg implements ArgSpec {

    private static final String ERR010 = "OptionalArg/010: Optional argument at position %d: %s";

    private TypeSpec type;
    private String specName;

    private Object defaultVal;
    private Object defaultGuardedVal;

    public OptionalArg(TypeSpec type, Object defaultValue) {
        this.type = type;
        this.defaultVal = defaultValue;
        specName = this.type.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] args, int pos, Context ctx)
    throws ArgSpecException {
        try {
            if (pos < 0 || pos >= args.length) {
                // Not enough arguments were provided, it can never contain the optional argument we are looking for.
                // Use the default value (after type coercion).
                return getDefaultVal(ctx);
            }
            else if (args[pos] instanceof Pair) {
                // Use the default value (after type coercion).
                // A pair is the start of the named arguments so we did not find the argument are looking for.
                return getDefaultVal(ctx);
            }
            else {
                // Coerce and check type.
                return type.guard(args[pos], ctx);
            }
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format(ERR010, pos, e.getMessage()));
        }
    }

    private Object getDefaultVal(Context ctx)
    throws TypeSpecException {
        if (defaultGuardedVal == null) {
            defaultGuardedVal = type.guard(defaultVal, ctx);
        }
        return defaultGuardedVal;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Arg{");
        sb.append("type=\"").append(specName).append("\"");
        sb.append(", default=\"").append(defaultVal).append("\"");
        sb.append('}');
        return sb.toString();
    }
}
