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
package branscha.scripty.parser;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a {@link Context} hierarchy. A new binding is always created in the top hierarchy,
 * when a binding is changed the binding is performed in the nearest context that contains
 * the binding.
 */
public class CompositeContext implements Context {

    private  static final String ERR010 = "CompositeContext/010: There is no binding for '%s' in the context.";

    private Context main, backing;

    /**
     * Construct a composite context.
     *
     * @param aChildCtx  The new topmost context.
     * @param aParentCtx Its parent.
     */
    public CompositeContext(Context aChildCtx, Context aParentCtx) {
        main = aChildCtx;
        backing = aParentCtx;
    }

    /**
     * A binding lookup starts from top (newest) to bottom (ancestors).
     */
    public Object getBinding(String key) {
        if (main.isBound(key)) return main.getBinding(key);
        else return backing.getBinding(key);
    }

    /**
     * Remove the most recent binding with that key.
     */
    public void removeBinding(String key) {
        if (main.isBound(key)) main.removeBinding(key);
        else backing.removeBinding(key);
    }

    /**
     * Change the nearest binding with  that key.
     */
    public void setBinding(String key, Object value)
    throws CommandException {
        if (main.isBound(key)) main.setBinding(key, value);
        else if (backing.isBound(key)) backing.setBinding(key, value);
        else{
            throw new CommandException(String.format(ERR010, key == null ? "null" : key));
        }
    }

    /**
     * Verify if there is a binding in the hierarchy.
     */
    public boolean isBound(String key) {
        return main.isBound(key) || backing.isBound(key);
    }

    /**
     * Find the root of the context tree, this is the oldest context.
     */
    public Context getRootContext() {
        if (backing != null) return backing.getRootContext();
        else if (main != null) return main.getRootContext();
        else return null;
    }

    /**
     * Create a new binding, this is always performed on the most recent context.
     */
    public void defBinding(String key, Object value) {
        main.defBinding(key, value);
    }

    public Map<String, Object> dumpBindings() {
        Map<String, Object> lBackingDump = backing.dumpBindings();
        Map<String, Object> lMainDump = main.dumpBindings();
        Map<String, Object> lDump = new HashMap<>(lBackingDump);
        lDump.putAll(lMainDump);
        return lDump;
    }

    @Override
    public String toString() {
        return main.toString() +
                "----------\n" +
                backing.toString();
    }
}