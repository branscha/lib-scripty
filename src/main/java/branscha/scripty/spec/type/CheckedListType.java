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
package branscha.scripty.spec.type;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import branscha.scripty.parser.IContext;

public class CheckedListType
        implements ITypeSpec {
    private ITypeSpec spec;
    private int min;
    private int max;

    public CheckedListType(ITypeSpec aSpec, int aMin, int aMax) {
        spec = aSpec;
        min = aMin;
        max = aMax;
    }

    public CheckedListType(ITypeSpec aSpec) {
        spec = aSpec;
        min = -1;
        max = -1;
    }

    public String getSpecName() {
        StringBuilder lBuilder = new StringBuilder();
        lBuilder.append("ListOf " + spec.getSpecName());
        if (min >= 0) lBuilder.append(", min size: ").append(min);
        if (max >= 0) lBuilder.append(", max size: ").append(max);
        return lBuilder.toString();
    }

    @SuppressWarnings("unchecked")
    public Object guard(Object aArg, IContext aCtx)
    throws TypeSpecException {
        if (!(aArg instanceof List))
            throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), aArg));
        final List lListArg = (List) aArg;

        if (min >= 0 && lListArg.size() < min)
            throw new TypeSpecException(String.format("Not enough elements in the list. There should be at least %d elements.", min));
        else if (max >= 0 && lListArg.size() > max)
            throw new TypeSpecException(String.format("Too many elements in the list. There should be at most %d elements.", max));

        try {
            // The list is modified in-place
            //
            final ListIterator lIter = lListArg.listIterator();
            while (lIter.hasNext()) {
                Object lObj = lIter.next();
                lIter.remove();
                lIter.add(spec.guard(lObj, aCtx));
            }
            return lListArg;
        }
        catch (UnsupportedOperationException e) {
            // We could not modify the list in-place ...
            // We recover by creating a new one in this case.
            //
            final List lNewList = new ArrayList(lListArg.size());
            for (Object lArg : lListArg) {
                lNewList.add(spec.guard(lArg, aCtx));
            }
            return lNewList;
        }
    }
}
