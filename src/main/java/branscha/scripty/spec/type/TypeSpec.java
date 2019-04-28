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
 * Specification of a type that is part of the Scripty type system. A TypeSpec is associated with an
 * {@link branscha.scripty.spec.args.ArgSpec} subclass by means of a {@link branscha.scripty.annot.ScriptyArg} annotation.
 * By associating a type with an argument description the eval engine can check the parameter passed to the command and
 * can do a conversion if necessary.
 *
 * It is the root of a complete type system.
 *
 * @param <T>
 */
public interface TypeSpec<T> {

    /**
     * Verify that an object satisfies the type. Try to convert the object into the required type.
     *
     * @param arg The object that needs conversion and verification.
     * @param ctx The eval context.
     * @return The checked object (which could have been converted).
     * @throws TypeSpecException
     */
    T guard(Object arg, Context ctx)
    throws TypeSpecException;

    String getSpecName();
}
