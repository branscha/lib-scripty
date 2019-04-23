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

import java.util.Map;

/**
 * Scripty data context.
 */
public interface Context {

    /**
     * Define a new binding. A binding must be defined before it can be changed.
     * Defining a binding more than once has the same effect sas setting the value multiple times.
     *
     * @param key Binding key.
     * @param value value.
     */
    public void defBinding(String key, Object value);

    /**
     * Change the binding, the binding must be defined.
     * @param key The binding key.
     * @param value The new value.
     * @throws CommandException The binding does not exist.
     */
    public void setBinding(String key, Object value)
    throws CommandException;

    /**
     * Retrieve a binding.
     * @param key Binding key.
     * @return The bound value or null if the key was not bound.
     */
    public Object getBinding(String key);

    /**
     * Verify if the binding exists.
     * @param key The binding key.
     * @return Flag that indicates if the binding exists.
     */
    public boolean isBound(String key);

    /**
     * Delete a binding.
     * @param key The binding key.
     */
    public void removeBinding(String key);

    /**
     * Get the oldest linked context.
     * @return
     */
    public Context getRootContext();

    /**
     * Create a map with a copy of all the bindings.
     * @return A map with all the bindings.
     */
    public Map<String, Object> dumpBindings();
}
