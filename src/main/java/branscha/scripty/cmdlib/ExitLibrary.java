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

import javax.swing.*;
import java.awt.*;

import static java.awt.GraphicsEnvironment.*;

@SuppressWarnings("unused")
@ScriptyLibrary(name = "System", type = ScriptyLibraryType.STATIC)
public class ExitLibrary {
    @ScriptyCommand(name="exit", description =
            "(exit [<exit-status>])\n" +
                    "Exit the application with optional exit status (0 by default).")
    @ScriptyStdArgList(optional = {@ScriptyArg(name = "status", type = "Integer", value = "0")})
    public static void exit(@ScriptyParam("status") Integer exitStatus) {
        final GraphicsEnvironment ge = getLocalGraphicsEnvironment();
        final boolean isHeadless = isHeadless();
        if (!isHeadless) {
            // We have to close all frames. Closing our main frame is not enough,
            // since the help system for instance creates other frames that have to be disposed as well.
            for (Frame frame : JFrame.getFrames()) frame.dispose();
        }
        else {
            System.exit(exitStatus);
        }
    }
}
