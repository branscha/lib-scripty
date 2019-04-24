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
package branscha.scripty.spec.args;

import branscha.scripty.parser.Context;

/**
 * Single argument specification. It can use all arguments to make a decision. Is aware of the location within the
 * arguments list. The argument specs are described by the {@link branscha.scripty.annot.ScriptyArg} annotation and
 * will eventually lead to the creation of a concrete subclass.
 *
 * The type checks are implemented in the ArgSpec implementations.
 *
 * @param <T>
 */
public interface ArgSpec<T> {

    /**
     * Verify the argument at that specified position.
     * @param aArgs The complete argument list.
     * @param aPos The index of the argument that should be verified.
     * @param aCtx The eval context.
     * @return The effective argument (which could be converted)
     * @throws ArgSpecException
     */
    T guard(Object[] aArgs, int aPos, Context aCtx)
    throws ArgSpecException;

    String getSpecName();
}
