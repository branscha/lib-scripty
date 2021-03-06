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
package branscha.scripty;

import branscha.scripty.cmdlib.*;
import branscha.scripty.cmdlib.gui.FileDialogLibrary;
import branscha.scripty.parser.CommandException;

import java.awt.*;

/**
 * A wrapper REPL that has two faces. If the environment is GUI capable, it will start a {@link ScriptyGuiRepl} but
 * if the runtime environment does not have graphical capabilities, it  starts a {@link ScriptyTextRepl}.
 */
public class ScriptyAutoRepl
extends ScriptyCapable {

    private ScriptyGuiRepl guiRepl;
    private ScriptyTextRepl textRepl;
    private boolean forceTextMode = false;

    public ScriptyAutoRepl() {
    }

    public ScriptyAutoRepl(ScriptyCapable aFacade) {
        super(aFacade);
    }

    private void buildRepl() {
        final boolean isHeadless = GraphicsEnvironment.isHeadless();
        if (isHeadless || forceTextMode) {
            textRepl = new ScriptyTextRepl(this);
        }
        else {
            guiRepl = new ScriptyGuiRepl(this);
        }
    }

    public void setForceTextMode(boolean aForceText) {
        forceTextMode = aForceText;
    }

    public void startLoop() {
        buildRepl();
        if (guiRepl != null) {
            guiRepl.startLoop();
        }
        else if (textRepl != null) {
            textRepl.startLoop();
        }
        else {
            throw new IllegalStateException("Could not create GUI nor TEXT REPL.");
        }
    }

    public static void main(String[] args)
    throws ExtensionException, CommandException {
        String mode = System.getProperty("mode", "gui");
        ScriptyAutoRepl repl = new ScriptyAutoRepl();

        // Load some of the command libraries.
        repl.setForceTextMode("text".equalsIgnoreCase(mode));
        repl.addLibraryClasses(
                PrintLibrary.class,
                MathLibrary.class,
                DebuggerLibrary.class,
                LoadLibrary.class,
                BeanLibrary.class,
                FileDialogLibrary.class,
                FileLibrary.class,
                HelpLibrary.class);

        // Load debugger macro's.
        repl.getReplEngine().exec("(load cp:/dbgutil.lsp)");

        // Start the interactive loop.
        repl.startLoop();
    }
}