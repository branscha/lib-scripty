/*
 * Scripty Programming Language
 * Copyright (C) 2010-2011 Bruno Ranschaert, S.D.I.-Consulting BVBA
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

package com.sdicons.repl.cmdlib;

import com.sdicons.repl.parser.*;
import com.sdicons.repl.spec.args.*;
import com.sdicons.repl.spec.type.ITypeSpec;
import com.sdicons.repl.spec.type.TypeSpecException;
import com.sdicons.repl.spec.type.TypeUtil;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * <ul>
 *    <li><b><code>load</code></b> Load and execute one or more scripts. If the file exists and is readable, it will be rememberd for the reload.<code>(load file | classpath:/resource | cp:/resource ...)</code></li>
 *    <li><b><code>reload</code></b> Reload previously loaded files. A file is remembered if it existed and was readable. <code>(reload)</code></li>
 * </ul>
 */
public class LoadCmd
extends AbstractCommand
{
    private static final String BINDING_LOADEDFILES = "*loaders";
    public static enum LoadCmdType {load, reload}

    private LoadCmdType type;

    public LoadCmd(LoadCmdType aType)
    {
        type = aType;
    }

    public static void registerCommands(IRegistry aReg)
    {
        aReg.registerCommand("load", new LoadCmd(LoadCmdType.load));
        aReg.registerCommand("reload", new LoadCmd(LoadCmdType.reload));
    }

    // TYPES
    ////////
    private static final ITypeSpec TLOADER = new LoaderType();
    // ARGS
    ////////
    private static final VarArg VLOADER = new VarArg(TLOADER);
    // PARAMETER LISTS
    //////////////////
    private static final IArgList VLOADER_ = new VarArgList(new FixedArg[]{}, VLOADER, 1, -1, new NamedArg[]{});

    @SuppressWarnings("unchecked")
    public Object execute(IEval aEval, IContext aContext, Object[] aArgs)
    throws CommandException
    {
        final List lLoaders = new ArrayList<Loader>();

        try
        {
            switch(type)
            {
                case load:
                {
                    Object[] lArgs = VLOADER_.guard(aArgs, aContext);
                    for(int i = 1; i < lArgs.length; i++) lLoaders.add(lArgs[i]);
                    break;
                }
                case reload:
                {
                    StdArgList.NOARG.guard(aArgs, aContext);
                    Object lLoaded = aContext.getRootContext().getBinding(BINDING_LOADEDFILES);
                    if(lLoaded == null || !(lLoaded instanceof List)) return null;
                    lLoaders.addAll((List)lLoaded);
                    break;
                }
                default:
                    throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[0]));
            }
        }
        catch (ArgSpecException e1)
        {
            throw new CommandException(String.format("ERROR: Command '%s' argument error.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e1)));
        }

        for(Object lCmdFileObj: lLoaders)
        {
            if(!(lCmdFileObj instanceof Loader))
                throw new CommandException(String.format("ERROR: The '%s' command expects a list of loaders in the context.\nEncountered instance of '%s'.", aArgs[0], lCmdFileObj==null?"null":lCmdFileObj.getClass().getCanonicalName()));
            final Loader lLoader = (Loader) lCmdFileObj;

            InputStream lIn;
            // Open the file.
            lIn = lLoader.getStream();
            // If we can open it, we remember it in the context.
            Object lLoaded = aContext.getRootContext().getBinding(BINDING_LOADEDFILES);
            if(lLoaded == null || !(lLoaded instanceof List))
            {
                // Binding did not exist.
                List<Loader> lNewLoaders = new ArrayList<Loader>();
                lNewLoaders.add(lLoader);
                aContext.getRootContext().defBinding(BINDING_LOADEDFILES, lNewLoaders);
            }
            else
            {
                // Binding existed.
                List lLoadedFiles = (List) lLoaded;
                if(lLoadedFiles.contains(lLoader))
                {
                    // Remove and add at the back, so that loading order is preserved.
                    lLoadedFiles.remove(lLoader);
                    lLoadedFiles.add(lLoader);
                }
                else lLoadedFiles.add(lLoader);
            }

            try
            {
                // Ok we can continue after all these checks.
                final Parser lParser = new Parser();
                final StreamBuffer lBuf = new StreamBuffer(lIn);
                while(!lBuf.eof())
                {
                    final Object lObj = lParser.parseExpression(lBuf);
                    if(lObj instanceof Token)
                    {
                        // If the parser returns a token and not an expression
                        // this means trouble.
                        Token lToken = (Token) lObj;
                        String lMsg = String.format("ERROR: While parsing '%s'.\n%s", lLoader.toString(), lToken.getValue());
                        throw new CommandException(lMsg);
                    }
                    aEval.eval(lObj);
                }
            }
            finally
            {
                // Cleanup. Ignore all exceptions here.
                if(lIn != null) try{lIn.close();}catch(Exception ignored){}
            }
        }
        return null;
    }
}

interface Loader
{
    InputStream getStream() throws CommandException;
    void checkValidity() throws CommandException;
}

class FileLoader
implements Loader
{
    private File file;

    public FileLoader(File aFile)
    {
        file = aFile;
    }

    public FileLoader(String aPath)
    {
        file = new File(aPath);
    }

    public void checkValidity() throws CommandException
    {
        if(!file.exists() || !file.isFile())
            throw new CommandException(String.format("The file does not exist: \"%s\".", file.getAbsolutePath()));
        if(!file.canRead())
            throw new CommandException(String.format("The file is not readable: \"%s\".", file.getAbsolutePath()));
    }

    public InputStream getStream()
    throws CommandException
    {
        try
        {
            return new FileInputStream(file);
        }
        catch (FileNotFoundException e)
        {
            throw new CommandException(String.format("ERROR: Exception while opening the file: '%s'.\n%s",file.getAbsolutePath(), e.getMessage()), e);
        }
    }

    @Override
    public String toString()
    {
        return file.getAbsolutePath();
    }
}

class ClasspathLoader
implements Loader
{
    String resource;

    public ClasspathLoader(String aResource)
    {
        resource = aResource;
    }

    public void checkValidity() throws CommandException
    {
        if (this.getClass().getResourceAsStream(resource) == null)
            throw new CommandException(String.format("The resource does not exist: \"%s\".", resource));
    }

    public InputStream getStream()
    throws CommandException
    {
        return this.getClass().getResourceAsStream(resource);
    }

    @Override
    public String toString()
    {
        return "classpath:" + resource;
    }
}

class LoaderType
implements ITypeSpec
{
    public String getSpecName()
    {
        return "loader";
    }

    public Object guard(Object aArg, IContext aCtx)
    throws TypeSpecException
    {
        Loader lCandLdr;
        if(aArg instanceof String)
        {
            String lPath = (String) aArg;
            if(lPath.startsWith("classpath:"))
            {
                lCandLdr = new ClasspathLoader(lPath.substring(10));
            }
            else if(lPath.startsWith("cp:"))
            {
                lCandLdr = new ClasspathLoader(lPath.substring(3));
            }
            else
            {
                // Interprete the string as a pathname.
                lCandLdr = new FileLoader(lPath);
            }
        }
        else if (aArg instanceof File)
        {
            // Easy for us.
            lCandLdr = new FileLoader((File) aArg);
        }
        else
        {
            // Don't know how to handle this.
            throw new TypeSpecException(TypeUtil.msgBadRepr(getSpecName(), aArg.toString()));                
        }

        try
        {
            lCandLdr.checkValidity();
        }
        catch (CommandException e)
        {
            throw new TypeSpecException(String.format("The resource '%s' is not available, it the contents cannot be accessed.", lCandLdr.toString()));
        }
        return lCandLdr;
    }
}
