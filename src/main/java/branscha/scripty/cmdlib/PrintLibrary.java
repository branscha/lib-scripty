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

import branscha.scripty.annot.ScriptyBindingParam;
import branscha.scripty.annot.ScriptyCommand;
import branscha.scripty.annot.ScriptyLibrary;
import branscha.scripty.annot.ScriptyLibraryType;

import java.io.PrintWriter;

@ScriptyLibrary(name = "System", type = ScriptyLibraryType.STATIC)
public class PrintLibrary {
    @ScriptyCommand
    public static String print(Object[] aArgs, @ScriptyBindingParam("*output") PrintWriter aWriter) {
        final String lResult = buildString(aArgs);
        if (aWriter != null) aWriter.print(lResult);
        return null;
    }

    @ScriptyCommand
    public static String println(Object[] aArgs, @ScriptyBindingParam("*output") PrintWriter aWriter) {
        final String lResult = buildString(aArgs);
        if (aWriter != null) aWriter.println(lResult);
        return null;
    }

    private static String buildString(Object[] aArgs) {
        final StringBuilder lBuilder = new StringBuilder();
        for (int i = 1; i < aArgs.length; i++) {
            final Object lObj = aArgs[i];
            if (lObj != null) {
                lBuilder.append(lObj.toString());
                if (i < aArgs.length - 1) lBuilder.append(" ");
            }
        }
        return lBuilder.toString();
    }
}
