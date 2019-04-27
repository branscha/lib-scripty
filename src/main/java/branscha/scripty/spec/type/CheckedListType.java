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
import java.util.ListIterator;

public class CheckedListType implements TypeSpec {

    private static final String ERR010 = "CheckedListType/010: Not enough elements in the list. There should be at least %d elements.";
    private static final String ERR020 = "CheckedListType/020: Too many elements in the list. There should be at most %d elements.";

    private TypeSpec spec;
    private int min;
    private int max;
    private String typeName;

    public CheckedListType(TypeSpec aSpec, String typeName, int aMin, int aMax) {
        spec = aSpec;
        this.typeName = typeName;
        min = aMin;
        max = aMax;
    }

    public CheckedListType(TypeSpec aSpec, int aMin, int aMax) {
        spec = aSpec;
        min = aMin;
        max = aMax;

        StringBuilder builder = new StringBuilder();
        builder.append("ListOf (" + spec.getSpecName()).append(")");
        if (min >= 0) builder.append(" ").append(min);
        if (max >= 0) builder.append(" ").append(max);
        typeName =  builder.toString();
    }

    public CheckedListType(TypeSpec aSpec) {
        spec = aSpec;
        min = -1;
        max = -1;
    }

    public String getSpecName() {
        return typeName;
    }

    @SuppressWarnings("unchecked")
    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {

        if (!(arg instanceof List))
            throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), arg));

        final List argLst = (List) arg;

        if (min >= 0 && argLst.size() < min)
            throw new TypeSpecException(String.format(ERR010, min));

        else if (max >= 0 && argLst.size() > max)
            throw new TypeSpecException(String.format(ERR020, max));

        try {
            // The list is modified in-place
            //
            final ListIterator iter = argLst.listIterator();
            while (iter.hasNext()) {
                Object obj = iter.next();
                iter.remove();
                iter.add(spec.guard(obj, ctx));
            }
            return argLst;
        }
        catch (UnsupportedOperationException e) {
            // We could not modify the list in-place ...
            // We recover by creating a new one in this case.
            //
            final List newList = new ArrayList(argLst.size());
            for (Object lArg : argLst) {
                newList.add(spec.guard(lArg, ctx));
            }
            return newList;
        }
    }
}
