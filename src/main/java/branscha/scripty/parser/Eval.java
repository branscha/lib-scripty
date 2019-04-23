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

/**
 * Evaluation of Scripty expressions. The core functionality of the Scripty framework.
 */
public interface Eval {

    /**
     * Retrieve the data context for the Eval.
     * @return The context.
     */
    Context getContext();

    /**
     * Set the data context for the Eval.
     */
    void setContext(Context context);

    /**
     * Evaluate an expression in the root REPL context.
     * This is the normal way of using the Eval.
     *
     * @param expr An expression.
     * @return The result of evaluation.
     * @throws CommandException
     */
    Object eval(Object expr)
    throws CommandException;

    /**
     * Evaluate an expression in an arbitrary context.
     * Commands could use this method to evaluated expressions internally.
     *
     * @param expr    An expression.
     * @param ctx An evaluation context.
     * @return The result of evaluation.
     * @throws CommandException
     */
    Object eval(Object expr, Context ctx)
    throws CommandException;

    /**
     * Provide the command repository. These commands can then be used in the Scripty expressions.
     * @param cmdRepository
     */
    public void setCommandRepo(CommandRepository cmdRepository);

    /**
     * Retrieve the command repository.
     * @return
     */
    CommandRepository getCommandRepo();

    /**
     * Provide the macro repository. These macro's can then be used in the Scripty expressions.
     * @param macroRepository
     */
    public void setMacroRepo(CommandRepository macroRepository);

    /**
     * Retrieve the macro's.
     * @return
     */
    CommandRepository getMacroRepo();
}