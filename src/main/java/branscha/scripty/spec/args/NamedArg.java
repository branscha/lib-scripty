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
import branscha.scripty.parser.Pair;
import branscha.scripty.spec.type.TypeSpec;
import branscha.scripty.spec.type.TypeSpecException;

public class NamedArg implements ArgSpec {

    private static final String ERR010 = "NamedArg/010: Named argument '%s': %s";

    private String paramName;
    private String paramFlag;
    private TypeSpec type;
    private boolean optional = true;
    private String specName;

    private Object defaultValue;
    private Object defaultGuardedVal;

    public NamedArg(String name, String flag, TypeSpec type, Object defaultValue, boolean optional) {
        this.paramName = name;
        this.paramFlag = flag;
        this.defaultValue = defaultValue;
        this.type = type;
        this.optional = optional;
        this.specName = this.type.getSpecName();
    }

    public String getSpecName() {
        return specName;
    }

    public Object guard(Object[] args, int pos, Context ctx)
    throws ArgSpecException {
        try {
            return type.guard(args[pos], ctx);
        }
        catch (TypeSpecException e) {
            throw new ArgSpecException(String.format(ERR010, paramName, e.getMessage()));
        }
    }

    Object getDefaultVal(Context ctx)
    throws TypeSpecException {
        if (defaultGuardedVal == null) {
            defaultGuardedVal = type.guard(defaultValue, ctx);
        }
        return defaultGuardedVal;
    }

    public String getName() {
        return paramName;
    }

    public boolean isOptional() {
        return optional;
    }

    public String getFlag() {
        return paramFlag;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Arg{");
        sb.append("name='").append(paramName).append('\'');
        if(paramFlag.length() > 0) sb.append(", flag=\"").append(paramFlag).append("\"");
        sb.append(", type=\"").append(specName).append("\"");
        if(defaultValue != null) sb.append(", default=\"").append(defaultValue).append("\"");
        sb.append(", optional=").append(optional);
        sb.append('}');
        return sb.toString();
    }
}
