/*******************************************************************************
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

import branscha.scripty.parser.IContext;
import branscha.scripty.parser.Pair;
import branscha.scripty.spec.type.ITypeSpec;
import branscha.scripty.spec.type.TypeSpecException;

public class NamedArg
        implements IArgSpec {
    private String paramName;
    private Object value;
    private ITypeSpec valueSpec;
    private boolean optional = true;
    private String specName;

    public NamedArg(String aName, ITypeSpec aValSpec, Object aValue, boolean aOptional) {
        paramName = aName;
        value = aValue;
        valueSpec = aValSpec;
        optional = aOptional;

        specName = paramName + "=" + valueSpec.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] aArgs, int aPos, IContext aCtx)
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
                        throw new ArgSpecException(String.format("Named argument '%s': %s", paramName, e.getMessage()));
                    }
                }
            }
        }

        try {
            if (optional) return valueSpec.guard(value, aCtx);
            else throw new ArgSpecException(String.format("Missing named argument '%s'.", paramName));
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(e.getMessage());
        }
    }

    public String getName() {
        return paramName;
    }
}
