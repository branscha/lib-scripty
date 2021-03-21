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

    public static final String ERR010 = "PartialMapping/010: Error fetching arguments sublist from %d length %d.";

    private int from;
    private int length;

    /**
     * Retrieve part of the arguments array. This is used to extract the variadic arguments into a separate array.
     *
     * @param from offset in the arguments array.
     * @param length the number of arguments to extract. If -1 then all the arguments from the offset will be copied.
     */
    public PartialMapping(int from, int length) {
        this.from = from;
        this.length = length;
    }

    public Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException {

        Object[] argsArray = (Object[]) args;
        int effectiveLength = 0;
        if (length < 0) {
            effectiveLength = argsArray.length - from;
            if (effectiveLength <= 0) effectiveLength = 0;
        }
        else {
            effectiveLength = length;
        }

        try {
            return Arrays.copyOfRange(argsArray, from, from + effectiveLength);
        }
        catch (Exception e) {
            throw new ArgMappingException(String.format(ERR010, from, effectiveLength));
        }
    }
}
