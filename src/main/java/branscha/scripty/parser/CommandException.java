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

public class CommandException extends Exception {
    private static final long serialVersionUID = -4301173601968482098L;

    // The stack will be filled in by the Eval2 implementation.
    // The Eval implementation does not manage a stack of its own, it will only
    // produce a message without actual stack.

    private Eval2.EvalStack stack;

    // Normal constructors without stack.
    // Used by the Eval implementation.

    public CommandException() {
        super();
    }

    public CommandException(String message, Throwable cause) {
        super(message, cause);
    }

    public CommandException(String message) {
        super(message);
    }

    public CommandException(Throwable cause) {
        super(cause);
    }

    // Stack constructors.
    // These are used by the Eval2 implementation.

    public CommandException(Eval2.EvalStack aStack) {
        super();
        this.stack = aStack;
    }

    public CommandException(String message, Throwable cause, Eval2.EvalStack aStack) {
        super(message, cause);
        this.stack = aStack;
    }

    public CommandException(String message, Eval2.EvalStack aStack) {
        super(message);
        this.stack = aStack;
    }

    public CommandException(Throwable cause, Eval2.EvalStack aStack) {
        super(cause);
        this.stack = aStack;
    }

    public Eval2.EvalStack getStack() {
        return stack;
    }

    public void setStack(Eval2.EvalStack stack) {
        this.stack = stack;
    }

    @Override
    public String getMessage() {
        // This method accomodates both Eval and Eval2 implementations.
        // If it is an exception from Eval, normally only the message will be available.
        // If it is a message from Eval2, we will include our own stacktrace.

        final String lMsg = super.getMessage();
        if (stack != null) return lMsg + "\n" + stack.toString();
        else return lMsg;
    }
}
