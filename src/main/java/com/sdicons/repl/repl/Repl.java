/*
 * Scripty Programming Language
 * Copyright (C) 2010-2012 Bruno Ranschaert, S.D.I.-Consulting BVBA
 * http://www.sdi-consulting.be
 * mailto://info@sdi-consulting.be
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package com.sdicons.repl.repl;

import com.sdicons.scripty.parser.*;

import java.io.*;
import java.util.List;

/**
 * The Repl understands a simplified lisp syntax, only strings and lists can be expressed
 * on the command line. Command evaluation can have any kind of Java object as a result.
 * <p>
 * Outermost parenthesis should be omitted so that the user is not set back by the lisp syntax.
 * <p>
 * Explicitly lacking: data structures, data structure manipulation. It should be done using Java commands and an
 * underlying Java model. The language on top should help to manipulate the underlying Java model.
 * <p>
 * Editing commands:
 * <ul>
 *    <li><b><code>break</code></b> will delete the current command that is being edited and 
 *    will start a fresh line.</li>
 *    <li>A line with a <b><code>backslash</code></b> will automatically continue with the next line. This allows for some
 *    nice formatting.</li>
 * </ul>
 *
 */
@Deprecated
public class Repl
implements IRepl
{
    private static final String INPUT = "*input";
    private static final String OUTPUT = "*output";
    private static final String ERROR = "*error";

    private IEval eval;
    private StringBuilder currCmd = new StringBuilder();
    private Parser parser = new Parser();
    private static final String PROMPT_NORMAL = "> ";
    private static final String PROMPT_CONTINUE = "+ > ";
    private String prompt = "> ";
    private LineNumberReader input;
    private PrintWriter output;
    private PrintWriter error;
    private boolean stopRequested = false;

    public Repl(InputStream aIn, OutputStream aOut, OutputStream aErr)
    {
        this(new BasicContext(), aIn, aOut, aErr);
    }

    public Repl(IContext aContext, InputStream aIn, OutputStream aOut, OutputStream aErr)
    {
        input = new LineNumberReader(new InputStreamReader(aIn));
        output = new PrintWriter(new OutputStreamWriter(aOut));
        error = new PrintWriter(new OutputStreamWriter(aErr));

        // Initialize our expression evaluator.
        eval = new Eval(aContext);

        // Provide the input and output streams in the context.
        aContext.getRootContext().defBinding(INPUT, input);
        aContext.getRootContext().defBinding(OUTPUT, output);
        aContext.getRootContext().defBinding(ERROR, error);
    }
    
    private void recover()
    {
        // Clear the current command, since we are done with it.
        currCmd.setLength(0);
        // Change the command prompt to normal.
        setPrompt(PROMPT_NORMAL);        
    }
    
    protected void handleLine(String aLine)
    {
        // Quickly return if there is nothing to do.
        if(aLine == null)
        {
            // We did not receive a line, so the user did not press enter.
            // This is because some error occurred.
            // So we must issue an extra newline.
            output.println();
            return;
        }
        if(aLine.trim().length() == 0)
            // We got an empty line, so the user pressed enter.
            return;
        
        // If the user doesn't know how to correct the command 
        // he can always start a new command on a fresh line.
        if("break".equals(aLine.trim().toLowerCase())) 
        {
            writeLine("Cancelling the command.");
            recover();  
            return;
        }
        
        if(aLine.endsWith("\\" ) && aLine.length() >= 2)
        {
        	// Line that ends with backslash, we will not attempt
        	// to parse the command.
        	currCmd.append(aLine.substring(0, aLine.length() - 1));
        	// Command not compete.
            setPrompt(PROMPT_CONTINUE);
        }
        else
        {
	        // Entering a command that consists of multiple lines.
	        // We test if the command can be parsed or not, and on that basis we decide if 
	        // the command is complete or not.
            // Keep the newlines so that we know which line produced the error.
	        currCmd.append(aLine).append("\n");
	        // Add implicit parenthesis if necessary.
	        String lCmd;
	        String lCurrCmdRepr = currCmd.toString().trim();
	        if(lCurrCmdRepr.startsWith("(") && lCurrCmdRepr.endsWith(")"))
	            // No need to add parenthesis, they are already there.
	            lCmd = lCurrCmdRepr;
	        else 
	            // Yes for the user convenience we add the parenthesis.
	            lCmd = "(" + currCmd.toString() + ")";        
	        Object lAst = parser.parseExpression(lCmd);
	        if(lAst instanceof List)
	        {                
	            recover();
	            // Time for evaluation.
	            try
	            {
	                eval.eval(lAst);
	            }
	            catch (CommandException e)
	            {
	            	// Normal command error.
	                writeErrorLine(e.getMessage());
	            }
	            catch(Exception e)
	            {
	            	// Probably a bug in a command.
	            	writeErrorLine("ERROR: Unexpected exception occurred.");
	            	writeErrorLine(e.getMessage());
	            }
	        }
	        else if(lAst instanceof Token)
	        {
	            // Command might not be complete.
	            Token lErr = (Token) lAst;
	            if(lErr.isEof())
	            {
	                // Command not compete.
	                setPrompt(PROMPT_CONTINUE);
	            }
	            else
	            {
	                // Real error, we have to recover.
	                recover();
	                writeLine(lErr.getValue());
	            }                
	        }
	        else
	        {
	            // Error.
	            recover();
	        }       
        }
    }

//    public static void main(String[] aArgs)
//    {
//        Repl lRepl = new Repl(System.in, System.out, System.out);
//        lRepl.registerCommand("print", new PrintCmd());
//        // For demo purposes only.
//        lRepl.registerCommand("uuid", new UuidCmd());
//        lRepl.registerCommand("choose-file", new FileChooserCmd(FileChooserType.files, FileChooserMode.open));
//        lRepl.registerCommand("choose-dir", new FileChooserCmd(FileChooserType.dirs, FileChooserMode.open));
//        lRepl.registerCommand("exit", new ExitCmd());
//        lRepl.registerCommand("load", new LoadCmd(LoadCmd.LoadCmdType.load));
//        lRepl.registerCommand("edit-rec", new RecEditCmd());
//        lRepl.registerCommand("format", new StrCmd(StrCmdType.strFormat));
//        lRepl.start();
//    }

    public void start()
    {
        while(!stopRequested)
        {
            final String lLine = readLine();
            handleLine(lLine);
        }
        stopRequested = false;
    }

    public void stop()
    {
        stopRequested = true;
    }

    private String readLine()
    {
        try
        {
            // Only write a prompt if there is no information to read
            // in the buffer. This could be the case when the user
            // pastes multiple lines, only the first will be read, and the
            // rest will be available in the buffer. We don't want to
            // write a prompt in this case.
            if(!input.ready())
            {
                write(prompt);
            }
            return input.readLine();
        }
        catch (IOException e)
        {
            return null;
        }
    }

    private void write(String aTxt)
    {
       output.print(aTxt);
       output.flush();
    }

    private void writeError(String aTxt)
    {
        error.print(aTxt);
        error.flush();
    }

    private void writeLine(String aLine)
    {
        output.println(aLine);
        output.flush();
    }

    private void writeErrorLine(String aLine)
    {
        error.println(aLine);
        error.flush();
    }

    public String getPrompt()
    {
        return prompt;
    }

    public void setPrompt(String prompt)
    {
        this.prompt = prompt;
    }

    public Object exec(String anExpression)
    throws CommandException
    {
        return eval.eval(parser.parseExpression(anExpression));
    }

    public IContext getContext()
    {
        return eval.getContext();
    }

    public void setContext(IContext aContext)
    {
        eval.setContext(aContext);
    }
}