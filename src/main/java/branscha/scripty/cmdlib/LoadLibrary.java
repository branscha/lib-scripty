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
@SuppressWarnings("unused")
@ScriptyLibrary(name="Load")
public class LoadLibrary {
    
    private static final String ERR010 = "LoadLibrary/010: While loading '%s'.%n%s";
    private static final String ERR020 = "LoadLibrary/020: The file does not exist: '%s'.";
    private static final String ERR030 = "LoadLibrary/030: The file is not readable: '%s'.";
    private static final String ERR040 = "LoadLibrary/040: Exception while opening the file: '%s'.%n%s";
    private static final String ERR050 = "LoadLibrary/050: The resource does not exist: '%s'.";
    private static final String ERR060 = "LoadLibrary/060: The resource cannot be opened: '%s'.%n%s";
    private static final String ERR070 = "LoadLibrary/070: The resource '%s' is not available, it the contents cannot be accessed.";
    
    private List<Loader> loaders = new ArrayList<>();

    @ScriptyCommand(name = "load", description =
            "(load <file> | classpath:/<resource> | cp:/<resource> ...)\n" +
                    "Load one or more scripts from files or resources.\n" +
                    "Also see: reload.")
    @ScriptyVarArgList(vararg = @ScriptyArg(name = "loaders", type = "Custom branscha.scripty.cmdlib.LoadLibrary$LoaderType"))
    public void load(@ScriptyParam("loaders") Object[] aLoaders, Context aCtx, Eval aEval)
    throws CommandException {
        List<Loader> lLoaders = new ArrayList<>();
        for (Object lLoader : aLoaders) {
            lLoaders.add((Loader) lLoader);
        }
        internalLoad(lLoaders, aEval, aCtx);
    }

    @ScriptyCommand(name = "reload", description =
            "(reload)\n" +
                    "Reload all the scripts that have been loaded with the 'load' command.\n" +
                    "Also see: load.")
    @ScriptyStdArgList()
    public void reload(Context aContext, Eval aEval)
    throws CommandException {
        List<Loader> lLoaders = new ArrayList<>(loaders);
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
                    final String lMsg = String.format(ERR010, lLoader.toString(), e.getMessage());
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

    public interface Loader {
        InputStream getStream()
        throws CommandException;

        void checkValidity()
        throws CommandException;
    }

    public static class FileLoader implements Loader {

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
                throw new CommandException(String.format(ERR020, file.getAbsolutePath()));
            if (!file.canRead())
                throw new CommandException(String.format(ERR030, file.getAbsolutePath()));
        }

        public InputStream getStream()
        throws CommandException {
            try {
                return new FileInputStream(file);
            }
            catch (FileNotFoundException e) {
                throw new CommandException(String.format(ERR040, file.getAbsolutePath(), e.getMessage()), e);
            }
        }

        @Override
        public String toString() {
            return file.getAbsolutePath();
        }
    }

    public static class ClasspathLoader implements Loader {

        String resource;

        public ClasspathLoader(String aResource) {
            resource = aResource;
        }

        public void checkValidity()
        throws CommandException {
            try (InputStream resourceStream = this.getClass().getResourceAsStream(resource)) {
                if (resourceStream == null)
                    throw new CommandException(String.format(ERR050, resource));
            }
            catch (IOException e) {
                throw new CommandException(String.format(ERR060, resource, e.getMessage()));
            }
        }

        public InputStream getStream() {
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
            return "Loader";
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
                throw new TypeSpecException(String.format(ERR070, lCandLdr.toString()));
            }
            return lCandLdr;
        }
    }
}
