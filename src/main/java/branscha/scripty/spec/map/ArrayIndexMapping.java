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
 * Fetch an argument from the argument array at the specified position. Position 0 is always the name of the command.
 * These mappings are automatically created by the {@link branscha.scripty.spec.args.ArgListBuilder} when the argument
 * list descriptions are converted to runtime information, and these are used by the {@link CmdMethodInjector} to inject
 * the requested arguments in command parameters.
 */
public class ArrayIndexMapping implements ArgMapping {

    public static final String ERR010 = "ArrayIndexMapping/010: Error while fetching argument %d from the argument array.";

    private int index;

    public ArrayIndexMapping(int index) {
        this.index = index;
    }

    @Override
    public Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException {
        try {
            return ((Object[]) args)[index];
        }
        catch (Exception e) {
            throw new ArgMappingException(String.format(ERR010, index));
        }
    }
}
