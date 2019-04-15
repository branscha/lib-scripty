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

public class InstanceOrBinding
        implements ITypeSpec {
    private ITypeSpec innerSpec;

    public InstanceOrBinding(ITypeSpec aSpec) {
        innerSpec = aSpec;
    }

    public Object guard(Object aArg, Context aCtx)
    throws TypeSpecException {
        try {
            if (aArg instanceof String && aCtx.isBound((String) aArg)) {
                aArg = aCtx.getBinding((String) aArg);
            }
            return innerSpec.guard(aArg, aCtx);
        }
        catch (TypeSpecException e) {
            // Convert the exception from the inner type spec to 
            // our own message. We hijack the exception here.
            throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), aArg), e);
        }
    }

    public String getSpecName() {
        return "Binding or " + innerSpec.getSpecName();
    }
}
