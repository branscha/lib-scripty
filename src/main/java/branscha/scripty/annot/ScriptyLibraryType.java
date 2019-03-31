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
package branscha.scripty.annot;

/**
 * The type of a {@link ScriptyLibrary}.
 * <ul>
 *     <li>INSTANCE: You have to create an instance and register it using {@link branscha.scripty.ScriptyCapable#addLibraryClasses(Class[])}. These commands can share data members.</li>
 *     <li>STATIC: You have to register the class using {@link branscha.scripty.ScriptyCapable#addLibraryInstances(Object...)}. The scripty engine will scan static methods in the class for command annotations. The library cannot store data locally to the library commands.</li>
 *     <li>AUTO: If you add an instance then Scripty will scan instance methods and if you register the class it will scan the static methods.</li>
 * </ul>
 */
public enum ScriptyLibraryType {
    INSTANCE,
    STATIC,
    AUTO
}
