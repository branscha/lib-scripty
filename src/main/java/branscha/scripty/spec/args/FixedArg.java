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
import branscha.scripty.spec.type.TypeSpec;
import branscha.scripty.spec.type.TypeSpecException;

/**
 * Represents a single fixed argument. Fixed arguments are positional (have a fixed location) and type. Their position
 * is enough to identify them so they don't need a name. They are required, the user has to provide them.
 */
public class FixedArg implements ArgSpec {

    private static final String ERR010 = "FixedArg/010: The required argument at position %d, type %s missing.";
    private static final String ERR020 = "FixedArg/020: The required argument at position %d: %s";

    private TypeSpec type;
    private String specName;

    public FixedArg(TypeSpec spec) {
        this.type = spec;
        specName = this.type.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] args, int pos, Context ctx)
    throws ArgSpecException {
        try {
            if (pos < 0 || pos >= args.length) {
                throw new ArgSpecException(String.format(ERR010, pos, type.getSpecName()));
            }
            else {
                // Coerce and check type.
                return type.guard(args[pos], ctx);
            }
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format(ERR020, pos, e.getMessage()));
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Arg{");
        sb.append("type=\"").append(specName).append("\"");
        sb.append('}');
        return sb.toString();
    }
}
