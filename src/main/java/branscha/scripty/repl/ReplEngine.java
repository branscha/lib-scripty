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
package branscha.scripty.repl;

import branscha.scripty.ExtensionException;
import branscha.scripty.ExtensionManager;
import branscha.scripty.parser.*;

import java.io.*;
import java.util.List;

/**
 * The Repl understands the Scripty command language.
 * <p>
 * Outermost parenthesis should be omitted so that the user is not set back by the Lisp syntax.
 * <p>
 * Explicitly lacking: data structures, data structure manipulation. It should be done using Java commands and an
 * underlying Java model. The language on top should help to manipulate the underlying Java model.
 * <p>
 * Editing commands:
 * <ul>
 * <li><b><code>break</code></b> will delete the current command that is being edited and will start a fresh line.</li>
 * <li>A line with a <b><code>backslash</code></b> will automatically continue with the next line. This allows for some
 * nice formatting.</li>
 * </ul>
 */
public class ReplEngine
implements ExtensionManager {

    private ExtensionRepositoryBuilder extensions = new ExtensionRepositoryBuilder();

    public static final String INPUT = "*input";
    public static final String OUTPUT = "*output";
    public static final String ERROR = "*error";

    private Context context;
    private Eval eval;
    private StringBuilder currCmd = new StringBuilder();
    private Parser parser = new Parser();
    private static final String PROMPT_NORMAL = "> ";
    private static final String PROMPT_CONTINUE = "+ > ";
    private String prompt = "> ";
    private LineNumberReader input;
    private PrintWriter output;
    private PrintWriter error;
    private boolean stopRequested = false;
    private boolean interactiveMode = true;
    private Object lastResult = null;

    /**
     * An empty implementation of {@link Writer} that does nothing.
     * Instances are used as default parameters if no streams are provided.
     */
    private static class NullWriter extends Writer {
        NullWriter() {
            super();
        }

        public void write(int c) {
        }

        public void write(char[] cbuf) {
        }

        public void write(String str) {
        }

        public void write(String str, int off, int len) {
        }

        public Writer append(CharSequence csq) {
            return this;
        }

        public Writer append(CharSequence csq, int start, int end) {
            return this;
        }

        public Writer append(char c) {
            return this;
        }

        public void write(char[] cbuf, int off, int len) {
        }

        public void flush() {
        }

        public void close() {
        }
    }

    private static final Writer NULLWRITER = new NullWriter();

    public ReplEngine() {
        context = new BasicContext();
        eval = new ClassicEval(context);
        eval.setCommandRepo(extensions.getCommandRepository());
        eval.setMacroRepo(extensions.getMacroRepository());
    }

    private void recover() {
        // Clear the current command, since we are done with it.
        currCmd.setLength(0);
        // Change the command prompt to normal.
        setPrompt(PROMPT_NORMAL);
    }

    protected void handleLine(String aLine)
    throws ReplEngineException {
        // Quickly return if there is nothing to do.
        if (aLine == null) {
            // We did not receive a line, so the user did not press enter.
            // This is because some error occurred.
            // So we must issue an extra newline.
            if (interactiveMode) output.println();
            return;
        }
        if (aLine.trim().length() == 0)
            // We got an empty line, so the user pressed enter.
            return;

        // If the user doesn't know how to correct the command 
        // he can always start a new command on a fresh line.
        if (interactiveMode && "break".equals(aLine.trim().toLowerCase())) {
            writeLine("Cancelling the command.");
            recover();
            return;
        }

        if (aLine.endsWith("\\") && aLine.length() >= 2) {
            // Line that ends with backslash, we will not attempt
            // to parse the command.
            currCmd.append(aLine, 0, aLine.length() - 1);
            // Command not compete.
            setPrompt(PROMPT_CONTINUE);
        }
        else {
            // Entering a command that consists of multiple lines.
            // We test if the command can be parsed or not, and on that basis we decide if
            // the command is complete or not.
            // Keep the newlines so that we know which line produced the error.
            currCmd.append(aLine).append("\n");
            // Add implicit parenthesis if necessary.
            String lCmd;
            String lCurrCmdRepr = currCmd.toString().trim();
            if (lCurrCmdRepr.startsWith("(") && lCurrCmdRepr.endsWith(")"))
                // No need to add parenthesis, they are already there.
                lCmd = lCurrCmdRepr;
            else
                // Yes for the user convenience we add the parenthesis.
                lCmd = "(" + currCmd.toString() + ")";
            Object lAst = parser.parseExpression(lCmd);
            if (lAst instanceof List) {
                recover();
                try {
                    // Time for evaluation!
                    lastResult = eval.eval(lAst);
                    // Save the last result in the context if not null.
                    if(lastResult != null) {
                        context.defBinding("_", lastResult);
                    }
                }
                catch (CommandException e) {
                    // Normal command error.
                    throw new ReplEngineException(input.getLineNumber(), 1, e.getMessage());
                }
            }
            else if (lAst instanceof Token) {
                // Command might not be complete.
                Token lErr = (Token) lAst;
                if (lErr.isEof()) {
                    // Command not compete.
                    setPrompt(PROMPT_CONTINUE);
                }
                else {
                    // Real error, we have to recover.
                    recover();
                    throw new ReplEngineException(input.getLineNumber(), lErr.getCol(), lErr.getValue());
                }
            }
            else {
                // Error.
                recover();
            }
        }
    }

    public void startInteractive() {
        // Interactive mode should only use out (not err) because otherwise the
        // two streams interfere with each other.
        this.startInteractive(System.in, System.out, System.out);
    }

    public void startInteractive(InputStream aIn, OutputStream aOut, OutputStream aErr) {
        interactiveMode = true;

        input = new LineNumberReader(new InputStreamReader(aIn));
        output = new PrintWriter(new OutputStreamWriter(aOut));
        error = new PrintWriter(new OutputStreamWriter(aErr));

        bindStreamsToContext();

        while (!stopRequested) {
            final String lLine = readLine();

            // Reset the line number if not accumulating a command.
            if (currCmd.length() <= 0) input.setLineNumber(1);

            try {
                handleLine(lLine);
            }
            catch (ReplEngineException e) {
                writeErrorLine(e.getMessage());
            }
            catch (Exception e) {
                // Probably a bug in a command.
                // A REPL has to keep on working, no matter what happens.
                writeErrorLine("ERROR: Unexpected exception occurred.");
                writeErrorLine(e.getMessage());
            }
        }
        stopRequested = false;
    }

    private void bindStreamsToContext() {
        context.getRootContext().defBinding(INPUT, input);
        context.getRootContext().defBinding(OUTPUT, output);
        context.getRootContext().defBinding(ERROR, error);
    }

    public Object startNonInteractive(String aExpr)
    throws ReplEngineException {
        return this.startNonInteractive(new StringReader(aExpr), NULLWRITER, NULLWRITER);
    }

    public Object startNonInteractive(InputStream aIn, OutputStream aOut, OutputStream aErr)
    throws ReplEngineException {
        return this.startNonInteractive(new InputStreamReader(aIn), new OutputStreamWriter(aOut), new OutputStreamWriter(aErr));
    }

    public Object startNonInteractive(InputStream aIn)
    throws ReplEngineException {
        return this.startNonInteractive(new InputStreamReader(aIn), NULLWRITER, NULLWRITER);
    }

    public Object startNonInteractive(Reader aIn)
    throws ReplEngineException {
        return startNonInteractive(aIn, NULLWRITER, NULLWRITER);
    }

    public Object startNonInteractive(Reader aIn, Writer aOut, Writer aErr)
    throws ReplEngineException {
        interactiveMode = false;
        lastResult = null;
        recover();

        input = new LineNumberReader(aIn);
        output = new PrintWriter(aOut);
        error = new PrintWriter(aErr);

        bindStreamsToContext();

        String lLine = readLine();
        while (!stopRequested && (lLine != null)) {
            handleLine(lLine);
            lLine = readLine();
        }
        if (!stopRequested) {
            if (currCmd.length() > 0) {
                // The stream did not finish cleanly, there is a residue
                // that cannot be interpreted as a command.
                // We should report on this.
                throw new ReplEngineException(input.getLineNumber(), 1, "ERROR: Syntax error:\n" + currCmd.toString());
            }
        }
        stopRequested = false;
        return lastResult;
    }

    public void stop() {
        stopRequested = true;
    }

    private String readLine() {
        try {
            // Only write a prompt if there is no information to read
            // in the buffer. This could be the case when the user
            // pastes multiple lines, only the first will be read, and the
            // rest will be available in the buffer. We don't want to
            // write a prompt in this case.
            if (interactiveMode && !input.ready()) {
                write(getPrompt());
            }
            return input.readLine();
        }
        catch (IOException e) {
            return null;
        }
    }

    private void write(String aTxt) {
        output.print(aTxt);
        output.flush();
    }

    private void writeError(String aTxt) {
        error.print(aTxt);
        error.flush();
    }

    private void writeLine(String line) {
        output.println(line);
        output.flush();
    }

    private void writeErrorLine(String aLine) {
        error.println(aLine);
        error.flush();
    }

    public String getPrompt() {
        return prompt;
    }

    public void setPrompt(String prompt) {
        this.prompt = prompt;
    }

    public Object exec(String anExpression)
    throws CommandException {
        return eval.eval(parser.parseExpression(anExpression));
    }

    public Context getContext() {
        return eval.getContext();
    }

    public void setContext(Context aContext) {
        // Remember the context at repl level.
        context = aContext;
        bindStreamsToContext();

        // Register the new context.
        eval.setContext(context);
    }

    public void addCommand(String aName, Command command) {
        extensions.addCommand(aName, command);
    }

    public void addMacro(String aName, Command macro) {
        extensions.addMacro(aName, macro);
    }

    public void addLibraryClasses(Class... libraryClasses)
    throws ExtensionException {
        extensions.addLibraryClasses(libraryClasses);
    }

    public void addLibraryInstances(Object... libraryInstances)
    throws ExtensionException {
        extensions.addLibraryInstances(libraryInstances);
    }

    public CommandRepository getCommandRepository() {
        return extensions.getCommandRepository();
    }

    public void setCommandRepository(CommandRepository aCommands) {
        extensions.setCommandRepository(aCommands);
    }

    public CommandRepository getMacroRepository() {
        return extensions.getMacroRepository();
    }

    public void setMacroRepository(CommandRepository aMacros) {
        extensions.setMacroRepository(aMacros);
    }
}
