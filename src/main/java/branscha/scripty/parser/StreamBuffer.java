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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

public class StreamBuffer
        implements IParserInput {
    private Reader reader;
    private char buff[] = new char[1024];
    private int buffPos = 0;
    private int buffLim = 0;
    private boolean eof = false;
    private int line = 1;
    private int col = 1;

    public StreamBuffer(InputStream aIn) {
        reader = new InputStreamReader(aIn);
    }

    public StreamBuffer(Reader aReader) {
        reader = aReader;
    }

    public char consumeChar() {
        fillBuf();
        if (eof) return 0;
        char lChar = buff[buffPos++];
        // Keep track of line/column count.
        if (lChar == '\n') {
            line++;
            col = 1;
        } else col++;
        return lChar;
    }

    public boolean eof() {
        fillBuf();
        return eof;
    }

    public int getColNr() {
        return col;
    }

    public int getLineNr() {
        return line;
    }

    public char peekChar() {
        fillBuf();
        if (eof) return 0;
        else return buff[buffPos];
    }

    private void fillBuf() {
        try {
            if (!eof && buffPos >= buffLim) {
                buffLim = reader.read(buff);
                if (buffLim == -1) {
                    eof = true;
                    buffPos = buffLim = 0;
                } else {
                    buffPos = 0;
                }
            }
        } catch (IOException e) {
            buffLim = buffPos = 0;
        }
    }
}
