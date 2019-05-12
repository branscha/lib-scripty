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

public class VarArg implements ArgSpec {

    private TypeSpec spec;
    private String specName;

    public VarArg(TypeSpec aSpec) {
        spec = aSpec;
        specName = spec.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] args, int pos, Context ctx)
    throws ArgSpecException {
        try {
            if (pos < 0 || pos >= args.length || args[pos] instanceof Pair)
                throw new ArgSpecException(String.format("Var argument at position %d: argument not present or type Pair found.", pos));
            else return spec.guard(args[pos], ctx);
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format("Var argument at position %d: %s", pos, e.getMessage()));
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
