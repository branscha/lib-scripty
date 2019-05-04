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

public class NamedArg implements ArgSpec {

    private static final String ERR010 = "NamedArg/010: Named argument '%s': %s";
    private static final String ERR020 = "NamedArg/020: Missing named argument '%s'.";

    private String paramName;
    private Object value;
    private TypeSpec valueSpec;
    private boolean optional = true;
    private String specName;

    public NamedArg(String aName, TypeSpec aValSpec, Object aValue, boolean aOptional) {
        paramName = aName;
        value = aValue;
        valueSpec = aValSpec;
        optional = aOptional;
        specName = valueSpec.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] aArgs, int aPos, Context aCtx)
    throws ArgSpecException {
        for (int i = aPos; i < aArgs.length; i++) {
            if (aArgs[i] instanceof Pair) {
                Pair lPair = (Pair) aArgs[i];
                if (paramName.equals(lPair.getLeft())) {
                    try {
                        Pair lNewPair = new Pair(paramName, valueSpec.guard(lPair.getRight(), aCtx));
                        aArgs[i] = lNewPair;
                        return lNewPair.getRight();
                    }
                    catch (TypeSpecException e) {
                        throw new ArgSpecException(String.format(ERR010, paramName, e.getMessage()));
                    }
                }
            }
        }

        try {
            if (optional) {
                return valueSpec.guard(value, aCtx);
            }
            else {
                throw new ArgSpecException(String.format(ERR020, paramName));
            }
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(e.getMessage());
        }
    }

    public String getName() {
        return paramName;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("Arg{");
        sb.append("name='").append(paramName).append('\'');
        sb.append(", type=\"").append(specName).append("\"");
        sb.append(", default=\"").append(value).append("\"");
        sb.append(", optional=").append(optional);
        sb.append('}');
        return sb.toString();
    }
}
