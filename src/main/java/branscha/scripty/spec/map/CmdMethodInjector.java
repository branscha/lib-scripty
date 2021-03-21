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

import java.util.ArrayList;
import java.util.List;

/**
 * A composite mapper that will inject the correct information in the command method parameters.
 * Some parameter types are automatically recognized by Scripty such as Eval, Context, Object[] and will
 * get the corresponding Scripty objects, and named arguments can also be injected in a parameter when it is
 * annotated with the argument name using {@link @branscha.scripty.annot.ScriptyParam}.
 */
public class CmdMethodInjector {

    private List<ArgMapping> mappings = new ArrayList<>();
    private ArgMapping[] compiledMappings = new ArgMapping[0];

    public void addArgMapping(ArgMapping argMapping) {
        mappings.add(argMapping);
        compiledMappings = mappings.toArray(new ArgMapping[]{});
    }

    public Object[] map(Eval eval, Context ctx, Object[] args)
    throws ArgMappingException {
        // Create a new method argument list.
        final Object[] inject = new Object[compiledMappings.length];
        // Inject each method argument with a piece of information from the Scripty context.
        for(int i = 0; i < compiledMappings.length; i++) {
            inject[i] = compiledMappings[i].map(eval, ctx, args);
        }
        return inject;
    }
}
