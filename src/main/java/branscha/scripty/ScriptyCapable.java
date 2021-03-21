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

import branscha.scripty.parser.Command;
import branscha.scripty.parser.Context;
import branscha.scripty.repl.ReplEngine;

/**
 * An embedding component that has a Scripty {@link ReplEngine} on board.
 */
public abstract class ScriptyCapable
implements ExtensionManager, ContextHolder {

    private ReplEngine replEngine;

    public ScriptyCapable() {
        replEngine = new ReplEngine();
    }

    /**
     * Initialize the Scripty shell based on the contents of an existing shell.
     * @param aFacade
     */
    public ScriptyCapable(ScriptyCapable aFacade) {
        replEngine = aFacade.getReplEngine();
    }

    protected ReplEngine getReplEngine() {
        return replEngine;
    }

    protected void setReplEngine(ReplEngine aEngine) {
        replEngine = aEngine;
    }

    public Context getContext() {
        return getReplEngine().getContext();
    }

    public void setContext(Context aContext) {
        getReplEngine().setContext(aContext);
    }

    public void addCommand(String aName, Command command)
    throws ExtensionException {
        getReplEngine().addCommand(aName, command);
    }

    public void addMacro(String aName, Command macro)
    throws ExtensionException {
        getReplEngine().addMacro(aName, macro);
    }

    public void addLibraryClasses(Class... libraryClasses)
    throws ExtensionException {
        getReplEngine().addLibraryClasses(libraryClasses);
    }

    public void addLibraryInstances(Object... libraryInstances)
    throws ExtensionException {
        getReplEngine().addLibraryInstances(libraryInstances);
    }
}
