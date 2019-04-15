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

import branscha.scripty.annot.ScriptyCommand;
import branscha.scripty.annot.ScriptyLibrary;
import branscha.scripty.annot.ScriptyLibraryType;
import branscha.scripty.parser.CommandException;
import branscha.scripty.parser.Pair;

/**
 * A Pair is an immutable parser artifact. There are only test functions and getter functions.
 * <ul>
 * <li><b><code>pair?</code></b> Test whether an object is of type pair.<code>(pair? obj)</code></li>
 * <li><b><code>pair-left</code></b> Get the left part of a pair.<code>(pair-left p)</code></li>
 * <li><b><code>pair-right</code></b> Get the right part of a pair.<code>(pair-right p)</code></li>
 * </ul>
 */
@ScriptyLibrary(type = ScriptyLibraryType.STATIC)
public class PairLibrary {
    @ScriptyCommand(name = "pair?")
    public static boolean isPair(Object[] aArgs)
    throws CommandException {
        return (guardSingleObject(aArgs) instanceof Pair);
    }

    @ScriptyCommand(name = "pair-left")
    public static Object pairLeft(Object[] aArgs)
    throws CommandException {
        return guardSinglePair(aArgs).getLeft();
    }

    @ScriptyCommand(name = "pair-right")
    public static Object pairRight(Object[] aArgs)
    throws CommandException {
        return guardSinglePair(aArgs).getRight();
    }

    private static Pair guardSinglePair(Object[] aArgs)
    throws CommandException {
        if (aArgs.length != 2 || !(aArgs[1] instanceof Pair))
            throw new CommandException(String.format("Command '%s' expects a single argument of type pair.", aArgs[0]));
        return (Pair) aArgs[1];
    }

    private static Object guardSingleObject(Object[] aArgs)
    throws CommandException {
        if (aArgs.length != 2)
            throw new CommandException(String.format("Command '%s' expects a single argument.", aArgs[0]));
        return aArgs[1];
    }
}
