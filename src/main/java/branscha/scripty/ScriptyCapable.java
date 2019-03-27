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
package branscha.scripty;

import branscha.scripty.parser.ICommand;
import branscha.scripty.parser.IContext;
import branscha.scripty.repl.ReplEngine;

public abstract class ScriptyCapable
implements IExtensions, IContextHolder
{
    private ReplEngine replEngine;

    public ScriptyCapable()
    {
        replEngine = new ReplEngine();
    }

    public ScriptyCapable(ScriptyCapable aFacade)
    {
        replEngine = aFacade.getReplEngine();
    }
    
    protected ReplEngine getReplEngine()
    {
        return replEngine;
    }

    protected void setReplEngine(ReplEngine aEngine)
    {
        replEngine = aEngine;
    }
    
    public IContext getContext()
    {
        return getReplEngine().getContext();
    }
    
    public void setContext(IContext aContext)
    {
        getReplEngine().setContext(aContext);
    }

    public void addCommand(String aName, ICommand aCommand)
            throws ExtensionException
    {
        getReplEngine().addCommand(aName, aCommand);
    }

    public void addMacro(String aName, ICommand aMacro)
    throws ExtensionException
    {
        getReplEngine().addMacro(aName, aMacro);
    }

    public void addLibraryClasses(Class... aLibraryClasses)
            throws ExtensionException
    {
        getReplEngine().addLibraryClasses(aLibraryClasses);
    }

    public void addLibraryInstances(Object... aLibraryInstances)
            throws ExtensionException
    {
        getReplEngine().addLibraryInstances(aLibraryInstances);
    }
}
