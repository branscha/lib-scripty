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

/**
 * An single argument mapping is a piece of code that knows how to fetch specific information from the
 * available data structures Eval, Context and the command arguments. For each command method, a {@link CmdMethodInjector} will be
 * created that contains an argument mapping for each parameter. Before the method is invoked we will call the
 * mappers to collect the correct arguments from the Scripty context.
 *
 * Also see {@link ResultMapping} which maps the result of a method invokation.
 */
public interface ArgMapping {

    /**
     * The mapper can make use of the eval itself, its context and all arguments in order to provide a value.
     *
     * @param eval The Scripty {@link Eval} engine.
     * @param ctx  The Scripty eval {@link Context}.
     * @param args The guarded (converted + augmented + checked) command arguments.
     * @return Some part of the available information that can be used somewhere.
     */
    Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException;

}
