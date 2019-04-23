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

import branscha.scripty.annot.ScriptyBindingParam;
import branscha.scripty.parser.Context;
import branscha.scripty.parser.Eval;

/**
 * This mapping fetches data from the {@link Context} of the {@link Eval} and provides it as an argument
 * of the {@link ScriptyBindingParam} annotated command parameter.
 */
public class BindingMapping implements ArgMapping {

    private String binding;
    private boolean excIfNull;

    public BindingMapping(String binding, boolean excIfNull) {
        this.binding = binding;
        this.excIfNull = excIfNull;
    }

    public Object map(Eval eval, Context ctx, Object args)
    throws ArgMappingException {
        if (ctx.isBound(binding)) return ctx.getBinding(binding);
        else if (excIfNull) throw new ArgMappingException("... no such binding ...");
        else return null;
    }

    public void setOffset(int aOffset) {
        // Nop.
    }
}
