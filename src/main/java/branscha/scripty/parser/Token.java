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

public class Token {
    // Difference between eof versus error.
    // We need this for the REPL. If a command is not complete, the user can continue 
    // the command on a fresh line. If there is an error, the message will be printed and a new
    // command will be started.
    public enum TokenType {
        beginlist, endlist, string, pair, whitespace, quote, error, eof
    }

    ;

    private String value;
    private TokenType type;

    private int line;
    private int col;

    public Token(TokenType aType, String aValue, int aLine, int aCol) {
        type = aType;
        value = aValue;
        line = aLine;
        col = aCol;
    }

    public TokenType getType() {
        return type;
    }

    public String getValue() {
        return value;
    }

    public boolean isWhitespace() {
        return type == TokenType.whitespace;
    }

    public boolean isErroneous() {
        return isError() || isEof();
    }

    public boolean isError() {
        return type == TokenType.error;
    }

    public boolean isEof() {
        return type == TokenType.eof;
    }

    public boolean isBeginList() {
        return type == TokenType.beginlist;
    }

    public boolean isEndList() {
        return type == TokenType.endlist;
    }

    public boolean isString() {
        return type == TokenType.string;
    }

    public boolean isQuote() {
        return type == TokenType.quote;
    }

    public boolean isPair() {
        return type == TokenType.pair;
    }

    public int getCol() {
        return col;
    }

    public int getLine() {
        return line;
    }
}
