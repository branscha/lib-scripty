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
import com.sdicons.repl.spec.type.*;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FileSysCmd
extends AbstractCommand
{
    public static enum FileSysCmdType {ls, cd, pwd, cat, rslv, mv, find}
    public static interface IFilter
    {
        boolean filter(String aPath, Object aValue);
    }

    private FileSysCmdType type;

    private static final String CURDIR = "*current-directory";
    private static final Pattern DRIVE = Pattern.compile("^\\p{Alpha}:.*$");

    // TYPES
    /////////
    // We accept both object of type File (from other commands) as well as string representations of file paths.
    private static final ITypeSpec TFILE = new OrType(new ITypeSpec[]{StringType.STRING_TYPE, new InstanceType(File.class, "file", false)});
    private static final ITypeSpec TLAMBDA = new InstanceOrBinding(new InstanceType(Lambda.class, false));

    // ARGS
    ////////
    // The optional named parameter quiet prevents output to the console. It can be useful in scripts.
    private static final NamedArg NQUIET = new NamedArg("quiet", BooleanType.BOOLEAN_TYPE, Boolean.FALSE, true);
    private static final NamedArg NINCLFILES = new NamedArg("files", BooleanType.BOOLEAN_TYPE, Boolean.TRUE, true);
    private static final NamedArg NINCLDIRS = new NamedArg("dirs", BooleanType.BOOLEAN_TYPE, Boolean.TRUE, true);
    private static final NamedArg NRECURSIVE = new NamedArg("recursive", BooleanType.BOOLEAN_TYPE, Boolean.FALSE, true);
    private static final NamedArg NGREP = new NamedArg("grep", StringType.STRING_TYPE, ".*", true);
    private static final NamedArg NEXEC = new NamedArg("exec", TLAMBDA, null, true);
    private static final OptionalArg OPATH = new OptionalArg(TFILE , ".");
    private static final FixedArg FPATH = new FixedArg(TFILE);

    // PARAMETER LISTS
    //////////////////
    // Single path argument and a quiet option.
    private static final IArgList OPATH_NQUIET_ = new StdArgList(new FixedArg[]{}, new OptionalArg[]{OPATH}, new NamedArg[]{NQUIET});
    private static final IArgList OPATH_NQUIET_NGREP_NINCFILES_NINCDIRS_NEXEC_ = new StdArgList(new FixedArg[]{}, new OptionalArg[]{OPATH}, new NamedArg[]{NQUIET, NGREP, NINCLFILES, NINCLDIRS, NEXEC, NRECURSIVE});
    // An empty argument list with the quiet option.
    private static final IArgList NOARG_NQUIET_ = new StdArgList(new FixedArg[]{}, new OptionalArg[]{}, new NamedArg[]{NQUIET});
    // Two paths and a quiet option.
    private static final IArgList FPATH_FPATH_NQUIET_ = new StdArgList(new FixedArg[]{FPATH, FPATH}, new OptionalArg[]{}, new NamedArg[]{NQUIET});

    public FileSysCmd(FileSysCmdType aType)
    {
        type = aType;
    }

    public static void registerCommands(IRegistry aReg)
    {
        // General notes:
        //  * It is intentionally based on the Java File object (for re-usability in other commands).
        //  * As a consequence, a move only accepts 2 files, no wildcards.
        //  * No wildcard globbing by the shell. The ls provides a grep on the short name. The find
        //    command provides a lookup facility.
        //  * Delete was not implemented for safety reasons.

        // Always quit. The option is allowed but does not have any effect.
        aReg.registerCommand("cd", new FileSysCmd(FileSysCmd.FileSysCmdType.cd));
        // Print the currrent directory and return the File object.
        aReg.registerCommand("pwd", new FileSysCmd(FileSysCmd.FileSysCmdType.pwd));
        // Print a listing of the current directory and return it als an array.
        //  - grep=regexp: applied to the absolute pathname.
        //  - quiet=true|*false.
        //  - files=*true|false : include files.
        //  - dirs=*true|false : include directories.
        //  - exec=lambda: process the files using a lambda. Processing is done after grepping and filtering.
        aReg.registerCommand("ls", new FileSysCmd(FileSysCmd.FileSysCmdType.ls));
        // Not implemented.
        aReg.registerCommand("cat", new FileSysCmd(FileSysCmd.FileSysCmdType.cat));
        // Resolve is the same as a 'quiet' pwd, but pwd only returns directories,
        // whereas rslv can also resolve to files.
        // This command is *very* important for integration with other libraries.
        // The other command libraries should not be dependent on this one, they
        // can simply request a File argument and this command can resolve the path to the File.
        aReg.registerCommand("rslv", new FileSysCmd(FileSysCmd.FileSysCmdType.rslv));
        // Rename a file.
        // It does not use wildcards sinds the implementation is based on the Java File class which
        // does not accept wildcards.
        aReg.registerCommand("mv", new FileSysCmd(FileSysCmd.FileSysCmdType.mv));
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        try
        {
            // Init stuff.
            initCurrDir(aCtx);

            // Get the printer from the context.
            final Object lWriterObj = aCtx.getBinding("*output");
            PrintWriter lWriter = null;
            if (lWriterObj instanceof PrintWriter) lWriter = (PrintWriter) lWriterObj;

            switch(type)
            {
                case mv:
                {
                    Object[] lArgs = FPATH_FPATH_NQUIET_.guard(aArgs, aCtx);

                    File lFrom;
                    if(lArgs[1] instanceof File) lFrom = (File) lArgs[1];
                    else lFrom = resolveFile((String) lArgs[1], aCtx);

                    File lTo;
                    if(lArgs[2] instanceof File) lTo = (File) lArgs[1];
                    else lTo = resolveFile((String) lArgs[2], aCtx);

                    boolean lResult = lFrom.renameTo(lTo);

                    if(!lResult)
                        throw new CommandException(String.format("ERROR: Command '%s' failed.", aArgs[0]));
                    return lTo;
                }
                case rslv:
                {
                    Object[] lArgs = OPATH_NQUIET_.guard(aArgs, aCtx);

                    File lFile;
                    if(lArgs[1] instanceof File) lFile = (File) lArgs[1];
                    else lFile = resolveFile((String) lArgs[1], aCtx);

                    return lFile;
                }
                case pwd:
                {

                    Object[] lArgs = NOARG_NQUIET_.guard(aArgs, aCtx);
                    boolean lQuiet = (Boolean) lArgs[1];
                    final File lCurDir = getCurrentDirectory(aCtx);

                    try
                    {
                        if(!lQuiet && lWriter != null) lWriter.println(lCurDir.getCanonicalPath());
                        return lCurDir;
                    }
                    catch (IOException e)
                    {
                        throw new CommandException(String.format("ERROR: Command '%s' cannot calculate the canonical name for '%s'.", aArgs[0], lCurDir), e);
                    }
                }
                case cd:
                {
                    Object[] lArgs = OPATH_NQUIET_.guard(aArgs, aCtx);
                    // boolean lQuiet = (Boolean) lArgs[2];

                    File lFile;
                    if(lArgs[1] instanceof File) lFile = (File) lArgs[1];
                    else lFile = resolveFile((String) lArgs[1], aCtx);

                    if(!lFile.isDirectory())
                        throw new CommandException(String.format("ERROR: Command '%s' cannot go to '%s', it is not a directory.", aArgs[0], lFile));

                    else aCtx.getRootContext().setBinding(CURDIR, lFile);
                    return lFile;
                }
                case ls:
                try
                {
                    Object[] lArgs = OPATH_NQUIET_NGREP_NINCFILES_NINCDIRS_NEXEC_.guard(aArgs, aCtx);
                    boolean lQuiet = (Boolean) lArgs[2];
                    String lGrep = (String) lArgs[3];
                    boolean lInclFiles = (Boolean) lArgs[4];
                    boolean lInclDirs = (Boolean)lArgs[5];
                    Lambda lLambda = (Lambda) lArgs[6];
                    boolean lRecursive = (Boolean) lArgs[7];

                    File lFile;
                    if(lArgs[1] instanceof File) lFile = (File) lArgs[1];
                    else lFile = resolveFile((String) lArgs[1], aCtx);

                    if(!lFile.isDirectory())
                        throw new CommandException(String.format("ERROR: Command '%s' cannot show '%s', it is not a directory.", aArgs[0], lFile));

                    // Calculate the grep pattern.
                    Pattern lPattern = Pattern.compile(lGrep);
                    // The worklist handles recursion.
                    List<File> lWorkList = new LinkedList<File>();
                    List<File> lResultList = new LinkedList<File>();
                    // Push our starting point on the directory worklist.
                    lWorkList.add(lFile);

                    while(lWorkList.size() > 0)
                    {
                        // Get one from the worklist.
                        File lDir = lWorkList.remove(0);
                        File[] lXfiles = lDir.listFiles();

                        // A guard, listing files can result in
                        // a null value ... meaning that there probably are
                        // security constraints on the directory.
                        if(lXfiles != null)
                        {
                            for(File lXFile: lXfiles)
                            {
                                if(!".".equals(lXFile.getName()) && !"..".equals(lXFile.getName()))
                                {
                                    if(lXFile.isDirectory())
                                    {
                                        // Remember if we want dirs.
                                        if(lInclDirs && lPattern.matcher(lXFile.getCanonicalPath()).matches()) lResultList.add(lXFile);
                                        // Add to the worklist if recursive.
                                        if(lRecursive) lWorkList.add(lXFile);
                                    }
                                    else if(lXFile.isFile())
                                    {
                                        // Remember if we want files.
                                        if(lInclFiles && lPattern.matcher(lXFile.getCanonicalPath()).matches()) lResultList.add(lXFile);
                                    }
                                }
                            }
                        }
                    }

                    for(File lEntry: lResultList)
                    {
                        if(lLambda != null) aEval.eval(lambdaMacro(lLambda, lFile), aCtx);

                        if (!lQuiet)
                        {
                            // Get basic file info.
                            String lDirFlag = lEntry.isDirectory()?"d":" ";
                            String lReadFlag = lEntry.canRead()?"r":" ";
                            String lWriteFlag = lEntry.canWrite()?"w":" ";
                            String lHiddenFlag = lEntry.isHidden()?"h":" ";
                            long lLastModified = lEntry.lastModified();
                            // Print the info.
                            if(lWriter != null) lWriter.println(String.format("%s%s%s%s  %5$tF %5$tR  %6$s", lDirFlag, lReadFlag, lWriteFlag, lHiddenFlag, lLastModified, lEntry.getCanonicalFile()));
                        }
                    }
                    return lResultList.toArray();
                }
                catch(IOException e)
                {
                    throw new CommandException(String.format("ERROR: Command '%s' internal error.\n%s", aArgs[0], e.getMessage()), e);
                }
                default:
                {
                    throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[0]));
                }
            }
        }
        catch (ArgSpecException e)
        {
            throw new CommandException(String.format("ERROR: Command '%s' argument error.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
    }

    @SuppressWarnings("unchecked")
    private List lambdaMacro(Lambda aLambda, File aFile)
    {
        List lMacro = new ArrayList();
        lMacro.add(aLambda);
        lMacro.add(aFile);
        return lMacro;
    }

    /**
     * Resolve a relative or absolute pathname.
     * It can be used by other command libraries as well.
     *
     * @param aPath
     * @param aCtx
     * @return
     * @throws CommandException
     */
    public static File resolveFile(String aPath, IContext aCtx)
    throws CommandException
    {
        // Check the setup. Other command libraries
        // might invoke this method directly, and in that case
        // this is the first initialization point.
        initCurrDir(aCtx);

        File lFile;
        final Matcher lMatcher = DRIVE.matcher(aPath);
        if(aPath.startsWith("/") || lMatcher.matches())
            // Absolute path when it does not start with a /
            // or when it does not start with a drive designation.
            lFile = new File(aPath);
        else
            // Relative path in all other cases.
            lFile = new File(getCurrentDirectory(aCtx), aPath);
        return lFile;
    }

    private static void initCurrDir(IContext aCtx)
    throws CommandException
    {
        if(!aCtx.isBound(CURDIR))
            aCtx.getRootContext().defBinding(CURDIR, new File("."));
        else
        {
            Object lPath = aCtx.getBinding(CURDIR);
            if(!(lPath instanceof File) || !((File)lPath).isDirectory())
                throw new CommandException(String.format("ERROR: Expected a File path and received an instance of '%s' value '%s'.", lPath==null?"null":lPath.getClass().getCanonicalName(), lPath==null?"null":lPath.toString()));
        }
    }

    /**
     * Get the current directory.
     * Other command libraries can make use of this as well.
     *
     * @param aCtx
     * @return
     * @throws CommandException
     */
    public static File getCurrentDirectory(IContext aCtx)
    throws CommandException
    {
        // Check the setup. Other command libraries
        // might invoke this method directly, and in that case
        // this is the first initialization point.
        initCurrDir(aCtx);

        if(aCtx.isBound(CURDIR))
        {
            Object lCand = aCtx.getBinding(CURDIR);
            if(lCand instanceof File)
            {
                File lDir = (File) lCand;
                if(lDir.isDirectory()) return lDir;
                else throw new CommandException("current dir does not contain a dir.");
            }
            else throw new CommandException("Current dir is not a File instance.");
        }
        else throw new CommandException("There is no current dir.");
    }
}