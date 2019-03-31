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
package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@ScriptyLibrary(name = "String", type = ScriptyLibraryType.STATIC)
@ScriptyNamedArgLists(
        std = {
                @ScriptyStdArgList(name = "1object", fixed = {@ScriptyArg(name = "arg", type = "Any nullAllowed=true")}),
                @ScriptyStdArgList(name = "1string", fixed = {@ScriptyArg(name = "arg", type = "String")}),
                @ScriptyStdArgList(name = "2strings", fixed = {@ScriptyArg(name = "arg1", type = "String"), @ScriptyArg(name = "arg2", type = "String")})
        },
        var = {
                @ScriptyVarArgList(name = "1string + objects", fixed = {@ScriptyArg(name = "str", type = "String")}, vararg = @ScriptyArg(name = "objs", type = "Any"))
        }
)
public class StringLibrary {
    @ScriptyCommand(name = "str?")
    @ScriptyRefArgList(ref = "1object")
    public static boolean isString(@ScriptyParam("arg") Object aArg) {
        return aArg instanceof String;
    }

    @ScriptyCommand(name = "str-trim")
    @ScriptyRefArgList(ref = "1string")
    public static String trim(@ScriptyParam("arg") String aArg) {
        return aArg.trim();
    }

    @ScriptyCommand(name = "str-format")
    @ScriptyRefArgList(ref = "1string + objects")
    public static String format(@ScriptyParam("str") String aFormat, @ScriptyParam("objs") Object[] aArgs) {
        return String.format(aFormat, aArgs);
    }

    @ScriptyCommand(name = "str-match")
    @ScriptyRefArgList(ref = "2strings")
    public static List<String> match(@ScriptyParam("arg1") String aPattern, @ScriptyParam("arg2") String aArg) {
        final Pattern lRegexp = Pattern.compile(aPattern);
        final Matcher lMatcher = lRegexp.matcher(aArg);
        final List<String> lRes = new ArrayList<String>();
        if (lMatcher.find()) {
            // Note: there is one more group then the count indicates.
            // This is because of group 0 which matches the complete string.
            int lCount = lMatcher.groupCount();
            for (int i = 0; i <= lCount; i++) lRes.add(lMatcher.group(i));
        }
        return lRes;
    }

    @ScriptyCommand(name = "str-match*")
    @ScriptyRefArgList(ref = "2strings")
    public static List<List<String>> matchRepeat(@ScriptyParam("arg1") String aPattern, @ScriptyParam("arg2") String aArg) {
        final Pattern lRegexp = Pattern.compile(aPattern);
        final Matcher lMatcher = lRegexp.matcher(aArg);
        final List<List<String>> lRes = new ArrayList<List<String>>();
        while (lMatcher.find()) {
            final List<String> lMatch = new ArrayList<String>();
            // Note: there is one more group then the count indicates.
            // This is because of group 0 which matches the complete string.
            int lCount = lMatcher.groupCount();
            for (int i = 0; i <= lCount; i++) lMatch.add(lMatcher.group(i));
            lRes.add(lMatch);
        }
        return lRes;
    }

    @ScriptyCommand(name = "str-match?")
    @ScriptyRefArgList(ref = "2strings")
    public static boolean isMatch(@ScriptyParam("arg1") String aPattern, @ScriptyParam("arg2") String aArg) {
        final Pattern lRegexp = Pattern.compile(aPattern);
        final Matcher lMatcher = lRegexp.matcher(aArg);
        return lMatcher.matches();
    }
}
