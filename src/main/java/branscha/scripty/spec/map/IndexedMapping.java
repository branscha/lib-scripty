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

import java.util.List;

/**
 * This mapper can fetch an object from a list or an array. It is mainly used to fetch a named argument from
 * the complete list of arguments if it is explicitly mapped to a command parameter (in stead of to the complete
 * list of arguments).
 */
public class IndexedMapping implements ArgMapping {

    private int offset;

    public IndexedMapping(int aOffset) {
        offset = aOffset;
    }

    public Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException {
        if (args == null) {
            // TODO generate ERROR
            throw new ArgMappingException("...");

        }

        if (offset < 0) {
            // TODO generate ERROR
            throw new ArgMappingException("...");
        }

        Class lArgsClass = args.getClass();
        if (lArgsClass.isArray()) {
            Object[] lArgs = (Object[]) args;
            if (offset >= lArgs.length) {
                // TODO generate ERROR
                throw new ArgMappingException("...");
            }

            return lArgs[offset];
        }
        else if (List.class.isAssignableFrom(lArgsClass)) {
            List lArgs = (List) args;
            if (offset >= lArgs.size()) {
                // TODO generate error
                throw new ArgMappingException("...");
            }
            return lArgs.get(offset);
        }

        // If we get here we could not interprete the argument object as an
        // indexed collection.
        throw new ArgMappingException("...");
    }

    public void setOffset(int aOffset) {
        offset = offset + aOffset;
    }
}
