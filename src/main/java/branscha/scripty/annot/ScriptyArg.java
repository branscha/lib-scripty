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

/**
 * Part of an argument list annotation. Provide argument details in an {@link ScriptyStdArgList} or al {@link ScriptyVarArgList}.
 * Argument list annotations are used to specify the interface for a command, the Scripty engine will automatically
 * enforce this interface.
 * <p>
 * The example defines a command interface with a single argument of type String.
 *
 * <pre>
 *    {@literal @}ScriptyStdArgList(fixed={{@literal @}ScriptyArg(name="uno", type="String")})
 * </pre>
 */
public @interface ScriptyArg {
    /**
     * The name of the argument. It can be used to map the argument to a command parameter using the
     * {@link ScriptyParam} annotation. For named parameters it is the name to use in the argument list.
     */
    String name();

    /**
     * The single letter flag representing the argument. Only for named parameters.
     * The user can use the --name with double dash or the -flag with single dash.
     * The flag cannot be a number.
     */
    String flag() default "";

    /**
     * The type of the argument, the description is in the type DSL (written in Scripty). The Scripty engine will
     * verify the argument type and will try to convert the argument before handing it over to the command.
     */
    String type();

    /**
     * The default value of the argument for named and optional parameters.
     */
    String value() default "";

    /**
     * Indicate if the  argument is optional or not for named parameters. Fixed parameters are obligatory, optional
     * parameters are optional by default and for named parameters you have a choice.
     */
    boolean optional() default false;
}
