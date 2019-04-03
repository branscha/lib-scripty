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
import branscha.scripty.spec.type.ITypeSpec;
import branscha.scripty.spec.type.TypeSpecException;

/**
 * Fixed arguments are positional (have a fixed location) and type.
 * They are required, the user has to provide them.
 */
public class FixedArg
        implements IArgSpec {
    private ITypeSpec spec;
    private String specName;

    public FixedArg(ITypeSpec aSpec) {
        spec = aSpec;
        specName = "fixed: " + spec.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] aArgs, int aPos, IContext aCtx)
    throws ArgSpecException {
        try {
            if (aPos < 0 || aPos >= aArgs.length)
                throw new ArgSpecException(String.format("The required argument at position %d, type %s missing.", aPos, spec.getSpecName()));
            else return spec.guard(aArgs[aPos], aCtx);
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format("The required argument at position %d: %s", aPos, e.getMessage()));
        }
    }
}
