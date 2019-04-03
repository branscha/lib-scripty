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
package branscha.scripty.spec.map;

import branscha.scripty.annot.ScriptyBindingParam;
import branscha.scripty.annot.ScriptyParam;
import branscha.scripty.parser.IContext;
import branscha.scripty.parser.IEval;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Map;

public class ArgMappingBuilderUtil {
    public static ArgListMapping buildArgMapping(Method aMethod, Map<String, IArgMapping> aMappings)
    throws ArgMappingException {
        Class<?>[] lParamTypes = aMethod.getParameterTypes();
        Annotation[][] lParamAnnotations = aMethod.getParameterAnnotations();

        ArgListMapping lArgListMapping = new ArgListMapping();

        for (int i = 0; i < lParamTypes.length; i++) {
            IArgMapping lMapping = buildMapping(lParamTypes[i], lParamAnnotations[i], aMappings);
            lArgListMapping.addArgMapping(lMapping);
        }
        return lArgListMapping;
    }

    private static IArgMapping buildMapping(Class<?> aParamClass, Annotation[] aParamAnnotations, Map<String, IArgMapping> aMappings)
    throws ArgMappingException {
        if (aParamAnnotations.length <= 0) {
            // If there are not parameter annotations, we check if
            // the parameter types is one of the well known types.

            if (aParamClass.isAssignableFrom(IEval.class)) {
                return new EvalMapping();
            }
            else if (aParamClass.isAssignableFrom(IContext.class)) {
                return new ContextMapping();
            }
            else if (aParamClass.isArray() && aParamClass.getComponentType().isAssignableFrom(Object.class)) {
                return new CompleteMapping();
            }
        }
        else {
            ScriptyParam lScriptyParamAnnot = null;
            ScriptyBindingParam lScriptyBindingAnnot = null;

            for (Annotation lAnnot : aParamAnnotations) {
                if (lAnnot instanceof ScriptyParam) lScriptyParamAnnot = (ScriptyParam) lAnnot;
                else if (lAnnot instanceof ScriptyBindingParam) lScriptyBindingAnnot = (ScriptyBindingParam) lAnnot;
            }

            if (lScriptyParamAnnot != null && lScriptyBindingAnnot != null) {
                throw new ArgMappingException("... there can only be one scripty param annotation ...");
            }
            else if (lScriptyParamAnnot != null) {
                String lParamName = lScriptyParamAnnot.value();
                if (aMappings != null && aMappings.containsKey(lParamName)) return aMappings.get(lParamName);
                else throw new ArgMappingException("... arg name not found ...");
            }
            else if (lScriptyBindingAnnot != null) {
                String lBindingName = lScriptyBindingAnnot.value();
                boolean lExcIfNull = lScriptyBindingAnnot.unboundException();
                return new BindingMapping(lBindingName, lExcIfNull);
            }
        }

        // Default is null value.
        return new NullMapping();
    }
}
