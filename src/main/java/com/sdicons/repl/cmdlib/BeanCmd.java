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

import java.beans.*;
import java.io.PrintWriter;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class BeanCmd
extends AbstractCommand
{
    public static enum BeanDirCmdType {ls, cd, pwd, cat, rslv, call, upd}
    public static interface IFilter
    {
        boolean filter(String aPath, Object aValue);
    }

    private BeanDirCmdType type;
    private IFilter[] filters = new IFilter[]{};

    private static final String CURDIR = "*current-bean";
    private static Pattern PAT_FIELDNAME_BIS =  Pattern.compile("^\\s*([^\\s\\[\\]]+)?\\s*(\\[\\s*([^\\s\\[\\]]+)\\s*\\])*\\s*$");

    private static class Resolution
    {
        private Object val;
        private String path;

        public Resolution(String aPath, Object aObj)
        {
            path = aPath;
            val = aObj;
        }

        public String getPath()
        {
            return path;
        }

        public Object getVal()
        {
            return val;
        }
    }

    public BeanCmd(BeanDirCmdType aType)
    {
        type = aType;
    }

    public BeanCmd(BeanDirCmdType aType, IFilter[] aFilters)
    {
        type = aType;
        filters = aFilters;
    }

    public static void registerCommands(IRegistry aReg)
    {
        // Goto another location. Absolute or relative paths allowed.
        // - / is the context of the repl.
        // - . and .. have normal meaning.
        // - [5] arrays, lists can be indexed.
        //   xyz[5] has same semantics as xyz/5
        aReg.registerCommand("bean-cd", new BeanCmd(BeanCmd.BeanDirCmdType.cd));
        // Print the current path to the repl.
        aReg.registerCommand("bean-pwd", new BeanCmd(BeanCmd.BeanDirCmdType.pwd));
        // List the contents of the denoted object.
        // The action depends on the type of the object, properties, array elements are listed.
        aReg.registerCommand("bean-ls", new BeanCmd(BeanCmd.BeanDirCmdType.ls));
        // Convert the denoted object to a string and print it on the repl.
        // The difference between ls and cat is that ls lists the subelements while
        // cat calls the toString() method.
        aReg.registerCommand("bean-cat", new BeanCmd(BeanCmd.BeanDirCmdType.cat));
        // Convert a pathname to the object itself.
        // We can obtain a direct reference to the object in this way.
        // It is what the other commands do automatically. This command is in fact the
        // link between other command libraries and this one.
        aReg.registerCommand("bean-rslv", new BeanCmd(BeanCmd.BeanDirCmdType.rslv));
        // Call java methods on an object denoted by a path.
        // The target object should be denoted by a pathname.
        // The method can be specied by a method instance or by a name (and a lookup will occur).
        // The arguments are not resolved, they are passed directly to the method. You can use the
        // rslv method above to accomplish argument resolution as well.
        aReg.registerCommand("bean-call", new BeanCmd(BeanCmd.BeanDirCmdType.call));
        aReg.registerCommand("bean-upd", new BeanCmd(BeanCmd.BeanDirCmdType.upd));
    }

    private void guardNoArgs(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length > 1)
            throw new CommandException("ERROR - no args expected.");
    }

    private String guardSingleString(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length == 2)
        {
            if(aArgs[1] instanceof String) return (String) aArgs[1];
            else throw new CommandException("ERROR - expected a string.");
        }
        else if(aArgs.length == 1)
        {
            return ".";
        }
        else throw new CommandException("ERROR - Too many args.");
    }

    private String guardStringOther(Object[] aArgs)
    throws CommandException
    {
        if(aArgs.length < 3)
            throw new CommandException("ERROR - Too few args.");
        if(!(aArgs[1] instanceof String))
            throw new CommandException("ERROR - expected a string as first arg.");
        return (String) aArgs[1];
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        // Initialize the directory system.
        // This ensures two things:
        // - The current path exists.
        // - The current path is a string.
        initCurrDir(aArgs, aCtx);

        // Get the printer from the context.
        final Object lWriterObj = aCtx.getBinding("*output");
        PrintWriter lWriter = null;
        if (lWriterObj instanceof PrintWriter) lWriter = (PrintWriter) lWriterObj;

        switch(type)
        {
            case upd:
            {
                final String lPath = guardStringOther(aArgs);
                final Resolution lRes = resolve(aArgs, lPath, aCtx);
                if(! filter(lRes)) throw new CommandException("ERROR: Cannot update filtered.");
                for(int i = 2; i < aArgs.length; i++)
                {
                    if(!(aArgs[i] instanceof Pair))
                        throw new CommandException("ERROR: pair expected.");
                    Pair lPair = (Pair) aArgs[i];
                    update(lRes.getVal(), lPair.getLeft(), lPair.getRight());
                }
            }
            case call:
            {
                try
                {
                    final String lPath = guardStringOther(aArgs);
                    final Resolution lRes = resolve(aArgs, lPath, aCtx);
                    if(! filter(lRes)) throw new CommandException("ERROR: Cannot call filtered.");
                    final Object lTarget = lRes.getVal();

                    Object[] lArgs = new Object[aArgs.length - 3];
                    for(int i = 0; i < lArgs.length; i++) lArgs[i] = aArgs[i + 3];

                    Method lMeth = null;
                    if(aArgs[2] instanceof Method)
                    {
                        lMeth = (Method) aArgs[2];
                    }
                    else if (aArgs[2] instanceof String)
                    {
                        Class[] lClasses = new Class[lArgs.length];
                        for(int i = 0; i < lArgs.length; i++) lClasses[i] = (lArgs[i]==null)?null:lArgs[i].getClass();

                        lMeth = lTarget.getClass().getMethod((String) aArgs[2], lClasses);
                    }

                    if(lMeth == null)
                        throw new CommandException("Method could not be resolved.");

                    return lMeth.invoke(lTarget, lArgs);
                }
                catch (Exception e)
                {
                    throw new CommandException("ERROR: invocation failed.\n" + e.getMessage(), e);
                }
            }
            case rslv:
            {
                // Fetch the path provided by the user.
                final String lPath = guardSingleString(aArgs);
                // Resolve the path.
                final Resolution lRes = resolve(aArgs, lPath, aCtx);
                if(! filter(lRes)) throw new CommandException("ERROR: Cannot rslv filtered.");
                else return lRes.getVal();
            }
            case cat:
            {
                // Fetch the path provided by the user.
                final String lPath = guardSingleString(aArgs);
                // Resolve the path.
                final Resolution lRes = resolve(aArgs, lPath, aCtx);
                if(! filter(lRes)) throw new CommandException("ERROR: Cannot cat filtered.");
                final Object lVal = lRes.getVal();
                if(lWriter != null) lWriter.println(lVal == null ? "null" : lVal.toString());
                return lVal;
            }
            case ls:
            {
                // Fetch the path provided by the user.
                final String lPath = guardSingleString(aArgs);
                // Resolve the path.
                final Resolution lRes = resolve(aArgs, lPath, aCtx);
                if(! filter(lRes)) throw new CommandException("ERROR: Cannot show filtered.");
                // Render the target object.
                final Object lVal = lRes.getVal();
                render(lVal, lWriter);
                return lVal;
            }
            case cd:
            {
                // Fetch the path provided by the user.
                final String lPath = guardSingleString(aArgs);;
                // Follow the path to see if it leads somewhere.
                final Resolution lRes = resolve(aArgs, lPath, aCtx);
                // We succeeded following the path, so it exists.
                if(!filter(lRes)) throw new CommandException("ERROR - cannot go there.");
                // We remember the current directory.
                aCtx.getRootContext().defBinding(CURDIR, lRes.getPath());
                return lRes.getVal();
            }
            case pwd:
            {
                guardNoArgs(aArgs);
                final Object lPath = aCtx.getBinding(CURDIR);
                if(lWriter != null) lWriter.println(lPath.toString());
                return lPath;
            }
            default:
                throw new CommandException(String.format("ERROR: Command '%s' internal error.", aArgs[0]));
        }
    }

    /**
     * Make sure that there is a "current directory" in the context
     * and that it is a Sting. We will not try to interprete other instance types.
     *
     * @param aArgs The arguments to the command.
     * @param aCtx The current context.
     * @throws CommandException The current directory is bound to an instance that cannot serve as a pathname.
     */
    private static void initCurrDir(Object[] aArgs, IContext aCtx)
    throws CommandException
    {
        if(!aCtx.isBound(CURDIR))
            aCtx.getRootContext().defBinding(CURDIR, "/");
        else
        {
            Object lPath = aCtx.getBinding(CURDIR);
            if(!(lPath instanceof String))
                throw new CommandException(String.format("ERROR: Command '%s' expects a string path and received an instance of '%s' value '%s'.", aArgs[0], lPath==null?"null":lPath.getClass().getCanonicalName(), lPath==null?"null":lPath.toString()));
        }
    }

    public static Resolution resolve(Object[] aArgs, String aPath, IContext aCtx)
    throws CommandException
    {
        initCurrDir(aArgs, aCtx);
        if(aPath.startsWith("/"))
        {
            final String lRelPath = aPath.substring(1);
            List<String> lPieces = canonize(lRelPath);

            Object lVal;
            if("".equals(lRelPath)) lVal = aCtx;
            else lVal = resolveCanonical(aArgs, lPieces, 0, aCtx);

            final StringBuilder lBuilder = new StringBuilder("/");
            for(String lPart : lPieces)
            {
                lBuilder.append(lPart);
                if(lPieces.get(lPieces.size() - 1) != lPart)
                    lBuilder.append("/");
            }
            return new Resolution(lBuilder.toString(), lVal);
        }
        else
        {
            final String lCurDir = (String) aCtx.getBinding(CURDIR);
            String lAbsPath;
            if("/".equals(lCurDir)) lAbsPath = lCurDir + aPath;
            else lAbsPath = lCurDir + "/" + aPath;
            return resolve(aArgs, lAbsPath, aCtx);
        }
    }

    private static Object resolveCanonical(Object[] aArgs, List<String> aPieces, int aCurrPiece, Object aBase)
    throws CommandException
    {
        if(aPieces.size() <= aCurrPiece) return aBase;
        final String lPiece = aPieces.get(aCurrPiece);
        int lIdx = -1;
        try
        {
            lIdx = Integer.parseInt(lPiece);
        }
        catch (NumberFormatException e1)
        {
            // Ignore this exception!
        }

        if(aBase instanceof IContext && lIdx < 0)
        {
            final IContext lCtx = (IContext) aBase;
            if(lCtx.isBound(lPiece))
                return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, lCtx.getBinding(lPiece));
            else
                throw new CommandException(String.format("ERROR: Command '%s' cannot find '%s' in context.", aArgs[0], lPiece));
        }
        else if(aBase instanceof Map && lIdx < 0)
        {
            final Map lMap = (Map) aBase;
            if(lMap.containsKey(lPiece))
                return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, lMap.get(lPiece));
            else
                throw new CommandException("ERROR - map.");
        }
        else if(aBase instanceof List)
        {
            final List lList = (List) aBase;
            if(lIdx < 0) throw new CommandException("List expects an index.");
            if(lIdx >= lList.size()) throw new CommandException("List index out of range.");
            else return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, lList.get(lIdx));
        }
        else  if(aBase != null)
        {
            if(aBase.getClass().isArray())
            {
                // Array rendering.
                ///////////////////

                if(lIdx >=0 && lIdx < Array.getLength(aBase))
                    return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, Array.get(aBase, lIdx));
                else throw new CommandException("ERROR - index out of range/wrong index type/no index.");
            }
            else
            {
                try
                {
                    // Try bean.
                    //
                    final BeanInfo lInfo = Introspector.getBeanInfo(aBase.getClass());
                    final PropertyDescriptor lProps[] = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps)
                    {
                        if (lProp.getName().equals(lPiece))
                        {
                            Method lGetter = lProp.getReadMethod();

                            Object[] lArgs;
                            if(lIdx >= 0) lArgs = new Object[]{lIdx};
                            else  lArgs = new Object[]{};

                            Object lVal = lGetter.invoke(aBase, lArgs);
                            return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, lVal);
                        }
                    }
                }
                catch(CommandException e)
                {
                    throw e;
                }
                catch (Exception e)
                {
                    throw new CommandException("ERROR - calling getter method.", e);
                }
            }

            // Oops, no property found.
            // What should we try next ?
            // TODO: alternative lookups.
            // - based on annotations.
            // - ...
            throw new CommandException("ERROR - no property.");
        }
        else if(aBase instanceof List && lIdx > 0)
        {
            List lList = (List) aBase;
            if(lIdx > lList.size() - 1)
                throw new CommandException("ERROR index out of bounds.");
            return resolveCanonical(aArgs, aPieces, aCurrPiece + 1, lList.get(lIdx));
        }
        else throw new CommandException("ERROR - null.");
    }

    private static List<String> canonize(String aPath)
    throws CommandException
    {
        final String[] lPieces = aPath.split("/");
        final List<String> lResult= new ArrayList<String>(lPieces.length);
        for(String lPiece : lPieces)
        {
            if(".".equals(lPiece))
                // Just skip this.
                continue;
            else if("..".equals(lPiece))
            {
                // Delete the last part and go back to the previous part.
                int lResultSize = lResult.size();
                if(lResultSize > 0) lResult.remove(lResultSize - 1);
            }
            else
            {
                // Note: Could be done more efficiently using a custom parser.
                final Matcher m = PAT_FIELDNAME_BIS.matcher(lPiece);
                if(!m.matches())
                    throw new CommandException(String.format("Illegal path '%s'.", lPiece));
                // Fetch the prefix.
                final String lName = m.group(1);
                if(lName != null) lResult.add(lName);
                // Get the indices.
                int lStart = m.end(1)<0?0:m.end(1);
                final String[] lIndices = lPiece.substring(lStart).split("\\[");
                // Skip the first empty string.
                for(int i = 1; i < lIndices.length; i++)
                {
                    String lIdx = lIndices[i];
                    int lClosing = lIdx.lastIndexOf(']');
                    if(lClosing > 0) lIdx = lIdx.substring(0, lClosing).trim();
                    lResult.add(lIdx);
                }
            }
        }
        return lResult;
    }

    @SuppressWarnings("unchecked")
    private void update(Object aTarget, Object aProp, Object aVal)
    throws CommandException
    {
        String lPropName = "";
        int lIdx = -1;

        if(aProp instanceof String) lPropName = (String) aProp;
        else throw new CommandException("ERROR: Don't know what to do with non-string keys ... (for now).");

        try
        {
            // Try to interprete the property as
            // an integer.
            lIdx = Integer.parseInt(lPropName);
        }
        catch (NumberFormatException e1)
        {
            // Ignore this exception!
        }

        if(aTarget instanceof List)
        {
            final List lTargetList = (List) aTarget;
            if(lIdx < 0)
                throw new CommandException("ERROR: Modifying list should use an index.");
            if(lIdx >= lTargetList.size())
                throw new CommandException("ERROR: Array index out of range.");

            // TODO: catch errors on this one.
            lTargetList.set(lIdx, aVal);

        }
        else if(aTarget != null)
        {
            if(aTarget.getClass().isArray())
            {
                // Array access.
                if(lIdx < 0)
                    throw new CommandException("ERROR: Modifying array should use an index.");
                if(lIdx >= Array.getLength(aTarget))
                    throw new CommandException("ERROR: Array index out of range.");

                // TODO: catch errors on this one.
                Array.set(aTarget, lIdx, aVal);
            }
            else
            {
                try
                {
                    // JavaBean access.
                    BeanInfo lInfo = Introspector.getBeanInfo(aTarget.getClass());
                    PropertyDescriptor lProps[] = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps)
                    {
                        if(lProp.getDisplayName().equals(lPropName))
                        {
                            Method lMeth = lProp.getWriteMethod();
                            if(lMeth == null)
                                throw new CommandException("ERROR: property cannot be written.");

                            if(lProp.getPropertyType() != aVal.getClass() && aVal instanceof String)
                            {
                                PropertyEditor lEditor = lProp.createPropertyEditor(aTarget);
                                lEditor.setAsText((String) aVal);
                            }
                            else lMeth.invoke(aTarget, new Object[]{aVal});
                        }
                    }
                }
                catch (Exception e)
                {
                    throw new CommandException("ERROR: " + e.getMessage(), e);
                }
            }
        }
    }

    private void render(Object aObj, PrintWriter aWriter)
    throws CommandException
    {
        if(aWriter == null) return;

        if(aObj instanceof IContext)
        {
            IContext lCtx = (IContext) aObj;
            Map<String, Object> lDump = lCtx.dumpBindings();
            for(String lKey : lDump.keySet())
            {
                Object lVal = lDump.get(lKey);
                aWriter.print(lKey);
                aWriter.print("=");
                aWriter.println(lVal == null ? "null" : lVal.toString());
            }
        }
        else if(aObj instanceof List)
        {
            // Show indexed ...
            aWriter.println(aObj.toString());
        }
        else if(aObj instanceof Map)
        {
            // Show map ....
            aWriter.println(aObj.toString());
        }
        else if(aObj != null)
        {
            if(aObj.getClass().isArray())
            {
                // Array rendering.
                ///////////////////

                aWriter.println(String.format("=== Type '%s' length %d ===", aObj.getClass().getSimpleName(), Array.getLength(aObj)));
                for(int i = 0; i < Array.getLength(aObj); i++)
                {
                    Object lObj = Array.get(aObj, i);
                    aWriter.println(String.format("%6s %s", "[" + i + "]", lObj==null?"null":lObj.toString()));
                }
            }
            else
            {
                try
                {
                    // Try JavaBean...
                    ///////////////////

                    aWriter.println(String.format("=== Type '%s' ===", aObj.getClass().getSimpleName()));
                    BeanInfo lInfo = Introspector.getBeanInfo(aObj.getClass());
                    PropertyDescriptor lProps[] = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps)
                    {
                        Method lRead = lProp.getReadMethod();
                        Method lWrite = lProp.getWriteMethod();
                        String lRw = "" + ((lRead!=null)?"r":"-") + ((lWrite!=null)?"w": "-");
                        aWriter.println(String.format("%s (%s, %s)", lProp.getDisplayName(), lProp.getPropertyType().getSimpleName(), lRw));
                    }
                }
                catch (IntrospectionException e)
                {
                    throw new CommandException("ERROR - ...", e);
                }
            }
        }
        else
            throw new CommandException("ERROR - null encountered.");
    }

    private boolean filter(Resolution aRes)
    {
        final String lPath = aRes.getPath();
        final Object lVal = aRes.getVal();

        for(IFilter lFilt: filters)
        {
            if(!lFilt.filter(lPath, lVal)) return false;
        }
        return true;
    }
}