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

public class OrType implements TypeSpec {

    private TypeSpec[] types;
    private String name;

    public OrType(TypeSpec[] types) {
        this.types = types;
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < this.types.length; i++) {
            buf.append(this.types[i].getSpecName());
            if (i < this.types.length - 1) buf.append(" or ");
        }
        name = buf.toString();
    }

    public String getSpecName() {
        return name;
    }

    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {
        for (int i = 0; i < types.length; i++) {
            try {
                return types[i].guard(arg, ctx);
            }
            catch (TypeSpecException e) {
                // Ignore, try the next one!   
                // Only one has to succeed.
            }
        }

        // If we arrive here, then none of the typespecs was true.
        throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), arg));
    }
}
