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
package branscha.scripty.parser;

import java.util.ArrayList;
import java.util.List;


/**
 * Internal data structure used by the Repl to represent user defined functions.
 */
public class Lambda {
    // The parameter names.
    private String[] params;
    // The function body.
    private Object expr;
    // Lexical context the context that was in effect
    // when the function was defined.
    private Context lexicalCtx;

    public Lambda(String[] aParams, Object aExpr, Context aLexicalCtx) {
        this.params = aParams;
        this.expr = aExpr;
        this.lexicalCtx = aLexicalCtx;
    }

    public Context createContext(Object[] aArgs, int aFrom, int aTo)
    throws CommandException {
        if ((aTo - aFrom) != params.length) {
            final List<String> lPretty = new ArrayList<String>(params.length);
            for (int i = 0; i < params.length; i++) lPretty.add(params[i]);
            throw new CommandException(String.format("Wrong number of arguments. Expected %d named %s and received %d.", params.length, lPretty, (aTo - aFrom)));
        }
        Context lCallContext = new BasicContext();
        for (int i = 0; i < params.length; i++) lCallContext.defBinding(params[i], aArgs[i + aFrom]);
        return new CompositeContext(lCallContext, lexicalCtx);
    }

    public Object getExpr() {
        return expr;
    }
}
