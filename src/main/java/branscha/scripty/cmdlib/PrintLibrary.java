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

import branscha.scripty.annot.*;

import java.io.PrintWriter;

@ScriptyNamedArgLists(
        var = {@ScriptyVarArgList(name = "string*",
                vararg = @ScriptyArg(name = "args", type = "String"),
                named = {@ScriptyArg(name = "delimiter", type = "String", optional = true, value = " "),
                        @ScriptyArg(name = "prefix", type = "String", optional = true, value = ""),
                        @ScriptyArg(name = "suffix", type = "String", optional = true, value = "")})}
)
@ScriptyLibrary(name = "System", type = ScriptyLibraryType.STATIC)
public class PrintLibrary {

    @ScriptyRefArgList(ref = "string*")
    @ScriptyCommand(name = "print", description =
            "(print str-1 ... str-n)\n" +
                    "Write the strings to *output, no return value.")
    public static String print(
            @ScriptyParam("args") Object[] args,
            @ScriptyBindingParam("*output") PrintWriter writer,
            @ScriptyParam("delimiter") String delimiter,
            @ScriptyParam("prefix") String prefix,
            @ScriptyParam("suffix") String suffix) {

        final String lResult = buildString(args, delimiter, prefix, suffix);
        if (writer != null) writer.print(lResult);
        return null;
    }

    @ScriptyRefArgList(ref = "string*")
    @ScriptyCommand(name = "println", description = "(println str-1 ... str-n)\n" +
            "Write the strings to *output followed by a new line, no return value.")
    public static String println(
            @ScriptyParam("args") Object[] args,
            @ScriptyBindingParam("*output") PrintWriter writer,
            @ScriptyParam("delimiter") String delimiter,
            @ScriptyParam("prefix") String prefix,
            @ScriptyParam("suffix") String suffix) {

        final String lResult = buildString(args, delimiter, prefix, suffix);
        if (writer != null) writer.println(lResult);
        return null;
    }

    private static String buildString(Object[] aArgs, String delimiter, String prefix, String suffix) {
        final StringBuilder builder = new StringBuilder(prefix);
        for (int i = 0; i < aArgs.length; i++) {
            final Object lObj = aArgs[i];
            if (lObj != null) {
                builder.append(lObj.toString());
                if (i < aArgs.length - 1) builder.append(delimiter);
            }
        }
        builder.append(suffix);
        return builder.toString();
    }
}
