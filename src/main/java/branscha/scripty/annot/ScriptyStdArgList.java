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
package branscha.scripty.annot;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An argument list definition which can be used to define the interface of a command. The type checks will be verified
 * when the command is invoked.
 *
 * A standard argument list with the structure:
 * <ul>
 * <li>A fixed number of required {@link ScriptyArg}.</li>
 * <li>A fixed number of optional {@link ScriptyArg}. If they are not provided, the default values will be used.</li>
 * <li>A number of named {@link ScriptyArg}. These can be specified to be optional or not.</li>
 * </ul>
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ScriptyStdArgList {
    /**
     * The name of the argument list so that it can be referenced later using {@link ScriptyRefArgList}.
     * The argument list can be defined before it is used (potentially multiple times).
     */
    String name() default "";

    /**
     * The array of positional argument descriptors. They are required and have a fixed location.
     * This is the standard type of parameter.
     */
    ScriptyArg[] fixed() default {};

    /**
     * The array of optional arguments which can have default values when not provided explicitly.
     * These are positional as well.
     */
    ScriptyArg[] optional() default {};

    /**
     * The named arguments are last and are provided using key=value pairs. The can be optional or
     * required, depending on the definition.
     */
    ScriptyArg[] named() default {};
}
