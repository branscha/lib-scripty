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
 * A type generator for instances of a class. Each class creates a new type.
 */
public class InstanceType implements TypeSpec {

    private static final String ERR010 = "InstanceType/010: Null values not allowed for type '%s'.";

    private Class clazz;
    private boolean allowsNull;
    private String typeName;

    /**
     * Construct a type from a Java class. The type name will be derived from the classname and the option to allow
     * or disallow nulls in the type.
     *
     * @param clazz       The java class.
     * @param nullAllowed Is it a nullable type or not.
     */
    public InstanceType(Class clazz, boolean nullAllowed) {
        this(clazz, "Instance " + clazz.getCanonicalName() + (nullAllowed ? " nullAllowed=true" : ""), nullAllowed);
    }

    /**
     * Construct a type from a Java class.
     *
     * @param clazz       The java class.
     * @param typeName    The explicitly provided type name.
     * @param nullAllowed Is it a nullable type or not.
     */
    public InstanceType(Class clazz, String typeName, boolean nullAllowed) {
        this.clazz = clazz;
        allowsNull = nullAllowed;
        this.typeName = typeName;
    }

    @SuppressWarnings("unchecked")
    public Object guard(Object arg, Context ctx)
    throws TypeSpecException {
        if (arg == null) {
            // I. null
            if (allowsNull) {
                return null;
            }
            else {
                throw new TypeSpecException(String.format(ERR010, getSpecName()));
            }
        }
        else {
            // II. non-null
            if (clazz.isAssignableFrom(arg.getClass())) {
                return arg;
            }
            else {
                try {
                    // Maybe we can convert the value to something appropriate.
                    return convertArg(arg);
                }
                catch (Exception e) {
                    // The conversion seems to have failed.
                    if (arg instanceof String) {
                        throw new TypeSpecException(TypeUtil.msgBadRepr(getSpecName(), (String) arg));
                    }
                    else {
                        throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), arg));
                    }
                }
            }
        }
    }

    Object convertArg(Object arg) {
        throw new IllegalArgumentException("Cannot convert.");
    }

    public String getSpecName() {
        return typeName;
    }
}