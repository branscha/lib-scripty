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

import branscha.scripty.parser.CommandException;
import branscha.scripty.parser.Context;

/**
 * Put the result of a command execution into the context.
 */
public class SetResultMapping implements ResultMapping {

    private  static final String ERR010 = "SetResultMapping/010: The result could not be bound to '%s' in the context, the binding does not exist.";

    private String binding;

    public SetResultMapping(String aBinding) {
        binding = aBinding;
    }

    public void map(Object cmdResult, Context ctx)
    throws ResultMappingException {
        try {
            ctx.setBinding(binding, cmdResult);
        }
        catch (CommandException e) {
            throw new ResultMappingException(String.format(ERR010, binding));
        }
    }
}
