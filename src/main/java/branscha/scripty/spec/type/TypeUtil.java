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

public class TypeUtil {
    public static final String msgExpectedOther(String aExpected, Object aObj) {
        String lMsg;
        if (aObj == null)
            lMsg = String.format("Expected type '%s' and received null value.", aExpected);
        else
            lMsg = String.format("Expected type '%s' and received incompatible type '%s' value '%s'.", aExpected, aObj.getClass().getCanonicalName(), aObj.toString());
        return lMsg;
    }

    public static final String msgBadRepr(String aExpected, String aRepr) {
        String lMsg;
        if (aRepr == null)
            lMsg = String.format("Expected type '%s' and received null value.", aExpected);
        else
            lMsg = String.format("Expected type '%s' and received incompatible string representation '%s'.", aExpected, aRepr);
        return lMsg;
    }
}
