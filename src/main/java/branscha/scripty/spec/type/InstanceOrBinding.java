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

/**
 * A type generator that accepts both variable names and instances. A variable name will be resolved
 * in the context and if it is not found it is converted to the inner type specification.
 */
public class InstanceOrBinding implements TypeSpec {

    private TypeSpec innerSpec;
    private String typeName;

    public InstanceOrBinding(TypeSpec innerSpec, String specName) {
        this.innerSpec = innerSpec;
        this.typeName = specName;
    }

    public InstanceOrBinding(TypeSpec aSpec) {
        innerSpec = aSpec;
        typeName = "InstanceOrBinding (" + innerSpec.getSpecName() + ")";
    }

    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {
        try {
            if (arg instanceof String && ctx.isBound((String) arg)) {
                arg = ctx.getBinding((String) arg);
            }
            return innerSpec.guard(arg, ctx);
        }
        catch (TypeSpecException e) {
            // Convert the exception from the inner type spec to 
            // our own message. We hijack the exception here.
            throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), arg), e);
        }
    }

    public String getSpecName() {
        return typeName;
    }
}
