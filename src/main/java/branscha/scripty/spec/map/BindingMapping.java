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
 * This mapping fetches data from the {@link Context} of the {@link Eval} and provides it as an argument
 * to a Script command method when it is part of a {@link  CmdMethodInjector}.
 */
public class BindingMapping implements ArgMapping {

    private  static final String ERR010 = "BindingMapping/010: The context does not contain a binding '%s' to inject into a command.";

    private String bindingKey;
    private boolean excIfNull;

    public BindingMapping(String bindingKey, boolean excIfNull) {
        this.bindingKey = bindingKey;
        this.excIfNull = excIfNull;
    }

    public Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException {
        if (ctx.isBound(bindingKey)) {
            return ctx.getBinding(bindingKey);
        }
        else if (excIfNull) {
            throw new ArgMappingException(String.format(ERR010, bindingKey));
        }
        else {
            return null;
        }
    }
}