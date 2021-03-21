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

import java.util.ArrayList;
import java.util.List;

/**
 * Type generator where the value can be one of the strings in the predefined set.
 */
public class EnumType implements TypeSpec {

    private List<String> values = new ArrayList<String>();
    private String typeName;

    public EnumType(List<String> values, String typeName) {
        this.values = values;
        this.typeName = typeName;
    }

    public EnumType(List<String> aValues) {
        values.addAll(aValues);

        StringBuilder buf = new StringBuilder("Enum");
        if(values.size() > 0) {
            values.stream().forEach((val)->buf.append(" ").append(val));
        }
        typeName = buf.toString();
    }

    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {
        if (arg == null || !values.contains(arg.toString())) {
            throw new TypeSpecException(TypeUtil.msgBadRepr(getSpecName(), (String) arg));
        }
        return arg.toString();
    }

    public String getSpecName() {
        return typeName;
    }
}
