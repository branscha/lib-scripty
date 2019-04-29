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
package branscha.scripty.spec.map;

import branscha.scripty.parser.Context;
import branscha.scripty.parser.Eval;

import java.util.Arrays;

/**
 * This mapping is used to fetch the variable argument list from the guarded args array and inject them into
 * an array parameter so that the var arg values are directly accessible. The current implementation only supports
 * array parameters.
 */
public class PartialMapping implements ArgMapping {

    private int from;
    private int length;

    public PartialMapping(int aFrom, int aLength) {
        from = aFrom;
        length = aLength;
    }

    public Object map(Eval eval, Context ctx, Object args) {
        Object[] lArgs = (Object[]) args;
        int lLength = 0;
        if (length < 0) {
            lLength = lArgs.length - from;
            if (lLength <= 0) lLength = 0;
        }
        return Arrays.copyOfRange(lArgs, from, from + lLength);
    }

    public void setOffset(int aOffset) {
        from = from + aOffset;
    }
}
