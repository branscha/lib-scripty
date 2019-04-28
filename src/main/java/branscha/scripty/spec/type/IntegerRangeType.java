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
package branscha.scripty.spec.type;

import branscha.scripty.parser.Context;

import static branscha.scripty.spec.type.IntegerType.INTEGER_TYPE;

public class IntegerRangeType implements TypeSpec {

    public static final String ERR010 = "IntegerRangeType/010: Value out of range. Expected type '%s' and received an incompatible type '%s' value '%s'.";

    private int from;
    private int to;
    private String typeName;

    public IntegerRangeType(int from, int to) {
        this.from = from;
        this.to = to;
        typeName = String.format("IntegerRange min=%d max=%d", from, to);
    }

    public String getSpecName() {
        return typeName;
    }

    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {
        final Integer value = (Integer) INTEGER_TYPE.guard(arg, ctx);
        if (value < from || value > to) {
            throw new TypeSpecException(String.format(ERR010, getSpecName(), arg.getClass().getCanonicalName(), arg.toString()));
        }
        else {
            return value;
        }
    }
}
