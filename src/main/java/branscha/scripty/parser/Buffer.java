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
package branscha.scripty.parser;

public class Buffer
        implements IParserInput {
    // Essential data.
    private String sentence;
    private int pos;
    // Informative bookkeeping.
    // Can be useful for error messages.
    private int line;
    private int col;

    public Buffer(String aSentence) {
        sentence = aSentence;
        pos = 0;

        line = 1;
        col = 1;
    }

    /* (non-Javadoc)
     * @see branscha.scripty.fileview.parser.IBuffer#consumeChar()
     */
    public char consumeChar() {
        if (eof()) return 0;
        else {
            // Get the character.
            final char lChar = sentence.charAt(pos++);
            // Keep track of line/column count.
            if (lChar == '\n') {
                line++;
                col = 1;
            } else col++;
            return lChar;
        }
    }

    /* (non-Javadoc)
     * @see branscha.scripty.fileview.parser.IBuffer#peekChar()
     */
    public char peekChar() {
        if (eof()) return 0;
        else return sentence.charAt(pos);
    }

    /* (non-Javadoc)
     * @see branscha.scripty.fileview.parser.IBuffer#eof()
     */
    public boolean eof() {
        return (pos >= sentence.length());
    }

    /* (non-Javadoc)
     * @see branscha.scripty.fileview.parser.IBuffer#getCol()
     */
    public int getColNr() {
        return col;
    }

    /* (non-Javadoc)
     * @see branscha.scripty.fileview.parser.IBuffer#getLine()
     */
    public int getLineNr() {
        return line;
    }
}
