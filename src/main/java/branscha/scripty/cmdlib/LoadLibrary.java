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
import branscha.scripty.parser.CommandException;
import branscha.scripty.parser.Context;
import branscha.scripty.parser.Eval;
import branscha.scripty.repl.ReplEngine;
import branscha.scripty.repl.ReplEngineException;
import branscha.scripty.spec.type.TypeSpec;
import branscha.scripty.spec.type.TypeSpecException;
import branscha.scripty.spec.type.TypeUtil;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * <ul>
 * <li><b><code>load</code></b> Load and execute one or more scripts. If the file exists and is readable, it will be rememberd for the reload.<code>(load file | classpath:/resource | cp:/resource ...)</code></li>
 * <li><b><code>reload</code></b> Reload previously loaded files. A file is remembered if it existed and was readable. <code>(reload)</code></li>
 * </ul>
 */
public class LoadLibrary {
    private List<Loader> loaders = new ArrayList<Loader>();

    @ScriptyCommand(name = "load")
    @ScriptyVarArgList(vararg = @ScriptyArg(name = "loaders", type = "Custom branscha.scripty.cmdlib.LoadLibrary$LoaderType"))
    public void load(@ScriptyParam("loaders") Object[] aLoaders, Context aCtx, Eval aEval)
    throws CommandException {
        List<Loader> lLoaders = new ArrayList<Loader>();
        for (Object lLoader : aLoaders) {
            lLoaders.add((Loader) lLoader);
        }
        internalLoad(lLoaders, aEval, aCtx);
    }

    @ScriptyCommand(name = "reload")
    @ScriptyStdArgList()
    public void reload(Context aContext, Eval aEval)
    throws CommandException {
        List<Loader> lLoaders = new ArrayList<Loader>();
        lLoaders.addAll(loaders);
        internalLoad(lLoaders, aEval, aContext);
    }

    private void internalLoad(List<Loader> aLoaders, Eval aEval, Context aContext)
    throws CommandException {
        Object lInput = aContext.getBinding(ReplEngine.INPUT);
        Object lOutput = aContext.getBinding(ReplEngine.OUTPUT);
        Object lError = aContext.getBinding(ReplEngine.ERROR);

        final ReplEngine lEngine = new ReplEngine();
        lEngine.setContext(aContext);
        lEngine.setCommandRepository(aEval.getCommandRepo());
        lEngine.setMacroRepository(aEval.getMacroRepo());

        try {
            for (Loader lLoader : aLoaders) {
                loaders.remove(lLoader);

                InputStream lIn = null;
                try {
                    lIn = lLoader.getStream();
                    lEngine.startNonInteractive(lIn);
                }
                catch (ReplEngineException e) {
                    final String lMsg = String.format("While loading '%s'.%n%s", lLoader.toString(), e.getMessage());
                    throw new CommandException(lMsg);
                }
                finally {
                    // Cleanup. Ignore all exceptions here.
                    if (lIn != null) try {
                        lIn.close();
                    }
                    catch (Exception ignored) {
                    }
                }

                loaders.add(lLoader);
            }
        }
        finally {
            aContext.setBinding(ReplEngine.INPUT, lInput);
            aContext.setBinding(ReplEngine.OUTPUT, lOutput);
            aContext.setBinding(ReplEngine.ERROR, lError);
        }
    }

    public static interface Loader {
        InputStream getStream()
        throws CommandException;

        void checkValidity()
        throws CommandException;
    }

    public static class FileLoader
            implements Loader {
        private File file;

        public FileLoader(File aFile) {
            file = aFile;
        }

        public FileLoader(String aPath) {
            file = new File(aPath);
        }

        public void checkValidity()
        throws CommandException {
            if (!file.exists() || !file.isFile())
                throw new CommandException(String.format("The file does not exist: \"%s\".", file.getAbsolutePath()));
            if (!file.canRead())
                throw new CommandException(String.format("The file is not readable: \"%s\".", file.getAbsolutePath()));
        }

        public InputStream getStream()
        throws CommandException {
            try {
                return new FileInputStream(file);
            }
            catch (FileNotFoundException e) {
                throw new CommandException(String.format("Exception while opening the file: '%s'.%n%s", file.getAbsolutePath(), e.getMessage()), e);
            }
        }

        @Override
        public String toString() {
            return file.getAbsolutePath();
        }
    }

    public static class ClasspathLoader
            implements Loader {
        String resource;

        public ClasspathLoader(String aResource) {
            resource = aResource;
        }

        public void checkValidity()
        throws CommandException {
            try (InputStream resourceStream = this.getClass().getResourceAsStream(resource)) {
                if (resourceStream == null)
                    throw new CommandException(String.format("The resource does not exist: \"%s\".", resource));
            }
            catch (IOException e) {
                throw new CommandException(String.format("The resource cannot be opened: \"%s\". %s", resource, e.getMessage()));
            }
        }

        public InputStream getStream()
        throws CommandException {
            return this.getClass().getResourceAsStream(resource);
        }

        @Override
        public String toString() {
            return "classpath:" + resource;
        }
    }

    @SuppressWarnings("unused")
    public static class LoaderType implements TypeSpec {

        public String getSpecName() {
            return "loader";
        }

        public Object guard(Object arg, Context ctx)
        throws TypeSpecException {
            Loader lCandLdr;
            if (arg instanceof String) {
                String lPath = (String) arg;
                if (lPath.startsWith("classpath:")) {
                    lCandLdr = new ClasspathLoader(lPath.substring(10));
                }
                else if (lPath.startsWith("cp:")) {
                    lCandLdr = new ClasspathLoader(lPath.substring(3));
                }
                else {
                    // Interprete the string as a pathname.
                    lCandLdr = new FileLoader(lPath);
                }
            }
            else if (arg instanceof File) {
                // Easy for us.
                lCandLdr = new FileLoader((File) arg);
            }
            else {
                // Don't know how to handle this.
                throw new TypeSpecException(TypeUtil.msgBadRepr(getSpecName(), arg.toString()));
            }

            try {
                lCandLdr.checkValidity();
            }
            catch (CommandException e) {
                throw new TypeSpecException(String.format("The resource '%s' is not available, it the contents cannot be accessed.", lCandLdr.toString()));
            }
            return lCandLdr;
        }
    }
}
