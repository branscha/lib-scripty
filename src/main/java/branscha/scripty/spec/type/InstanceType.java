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
package branscha.scripty.spec.type;

import branscha.scripty.parser.IContext;

public class InstanceType
implements ITypeSpec
{
    private Class requiredClass;
    private boolean allowsNull = false;
    private String typeName;
    
    public InstanceType(Class aClass, boolean nullAllowed)
    {
        this(aClass, aClass.getCanonicalName(), nullAllowed);
    }
    
    public InstanceType(Class aClass, String aTypeName, boolean nullAllowed)
    {
        requiredClass = aClass;       
        allowsNull = nullAllowed;
        typeName = aTypeName;
    }
    
    @SuppressWarnings("unchecked")
    public Object guard(Object aArg, IContext aCtx)
    throws TypeSpecException
    {
        if(aArg == null)
        {
            if(allowsNull) return null;
            else throw new TypeSpecException(String.format("Null value. Expected type '%s' and null is not allowed.", typeName));
        }
        else
        {
            if(requiredClass.isAssignableFrom(aArg.getClass())) return aArg;
            else throw new TypeSpecException(TypeUtil.msgExpectedOther(getSpecName(), aArg));
        }        
    }
    
    public String getSpecName()
    {
        return typeName;
    }
}
