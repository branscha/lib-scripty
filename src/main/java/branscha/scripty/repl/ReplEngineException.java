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
package branscha.scripty.repl;

public class ReplEngineException extends Exception {
    private int line = 1;
    private int column = 1;

    public ReplEngineException() {
        super();
    }

    public ReplEngineException(int aLine, int aColumn, String message) {
        super(message);
        line = aLine;
        column = aColumn;
    }

    public ReplEngineException(int aLine, int aColumn, String message, Throwable cause) {
        super(message, cause);
        line = aLine;
        column = aColumn;
    }

    public ReplEngineException(int aLine, int aColumn, Throwable cause) {
        super(cause);
        line = aLine;
        column = aColumn;
    }

    @Override
    public String getMessage() {
        return String.format("ERROR: On line %d column %d.%n%s", line, column, super.getMessage());
    }
}
