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
package branscha.scripty.cmdlib;

public class CmdUtil {
    /**
     * Concatenate error messages from nested exceptions to get an error message that contains
     * as much information as possible.
     *
     * @param e An exception.
     * @return An exception message that is the concatenation of all embedded exception messages.
     */
    public static String concatExceptionMessages(Throwable e) {
        final StringBuilder lBuilder = new StringBuilder();
        if (e.getMessage() != null) lBuilder.append(e.getMessage());
        if (e.getCause() != null) lBuilder.append("\n");
        Throwable t = e;
        while (t.getCause() != null) {
            t = t.getCause();
            if (t.getMessage() != null)
                lBuilder.append(t.getMessage());
            if (t.getCause() != null) lBuilder.append("\n");
        }
        return lBuilder.toString();
    }
}
