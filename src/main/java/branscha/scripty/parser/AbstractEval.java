/*******************************************************************************
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

import java.util.Collection;

public abstract class AbstractEval
        implements IEval {
    private IContext context;

    public AbstractEval(IContext aContext) {
        context = aContext;
    }

    public IContext getContext() {
        return context;
    }

    public void setContext(IContext context) {
        this.context = context;
    }

    @SuppressWarnings("unchecked")
    protected static boolean boolEval(Object aObj) {
        if (aObj instanceof Boolean) return (Boolean) aObj;
        else if (aObj instanceof Collection) return ((Collection) aObj).size() > 0;
        else if (aObj instanceof Byte) return ((Byte) aObj).intValue() != 0;
        else if (aObj instanceof Short) return ((Short) aObj).intValue() != 0;
        else if (aObj instanceof Integer) return (Integer) aObj != 0;
        else if (aObj instanceof Long) return (Long) aObj != 0l;
        else if (aObj instanceof String) {
            String lNor = ((String) aObj).toLowerCase();
            return ("true".equals(lNor) || "yes".equals(lNor) || "t".equals(lNor) || "y".equals(lNor) || "on".equals(lNor));
        }
        else return aObj != null;
    }
}
