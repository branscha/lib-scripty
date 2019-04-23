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
 * Food for the parser.
 */
public interface ParserInput {

    /**
     * Read and remove the character from the input.
     *
     * @return The next character that is read.
     */
    char consumeChar();

    /**
     * Inspect the next character that could be read, but do not
     * remove it from the stream. Just look at it to make a decision.
     *
     * @return The next character that will be read.
     */
    char peekChar();

    /**
     * Test if there still are characters on the stream.
     *
     * @return
     */
    boolean eof();

    /**
     * Currrent column, the number of characters since the last newline was encountered.
     * Counting starts at 1 (user oriented).
     *
     * @return
     */
    int getColNr();

    /**
     * Get the number of lines (separated by newlines). Counting starts at 1 (user friendliness).
     *
     * @return
     */
    int getLineNr();
}
