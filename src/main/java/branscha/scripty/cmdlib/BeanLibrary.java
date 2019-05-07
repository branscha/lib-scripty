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
import branscha.scripty.parser.Pair;

import java.beans.*;
import java.io.PrintWriter;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("unused")
@ScriptyLibrary(name = "JavaBean", type = ScriptyLibraryType.INSTANCE)
@ScriptyNamedArgLists(
        std = {
                @ScriptyStdArgList(name = "path", optional = {@ScriptyArg(name = "path", type = "String", value = ".")}),
                @ScriptyStdArgList(name = "path + quiet option",
                        named = {@ScriptyArg(name = "quiet", type = "Boolean", optional = true, value = "false")},
                        optional = {@ScriptyArg(name = "path", type = "String", value = ".")})},
        var = {
                @ScriptyVarArgList(name = "call",
                        fixed = {@ScriptyArg(name = "bean", type = "Any"), @ScriptyArg(name="methodName", type = "String")},
                        vararg = @ScriptyArg(name = "varargs", type = "Any"))
        }
)
public class BeanLibrary {

    private String currentDirectory = "/";

    // Goto another location. Absolute or relative paths allowed.
    // - / is the context of the repl.
    // - . and .. have normal meaning.
    // - [5] arrays, lists can be indexed.
    //   xyz[5] has same semantics as xyz/5
    //
    @ScriptyCommand(name = "bean-cd", description =
            "(bean-cd / | .. | . | <property-path> | <array-property>/<index> | <array-property>[<index>] )\n" +
                    "Traverse the properties in the context hierarchy like a directory tree.\n" +
                    "Also see: bean-ls, bean-cat, bean-pwd.")
    @ScriptyStdArgList(fixed = {@ScriptyArg(name = "path", type = "String")})
    public Object beanCd(@ScriptyParam("path") String aPath, Context aCtx)
    throws CommandException {
        // Follow the path to see if it leads somewhere.
        final Resolution resolution = resolve(aPath, aCtx);
        // We succeeded following the path, so it exists.
        // We remember the current directory.
        currentDirectory = resolution.getPath();
        return resolution.getVal();
    }

    // Print the current path to the repl.
    //
    @ScriptyCommand(name = "bean-pwd",
            description = "(bean-pwd)\n" +
                    "Write the current property path to *output and return the path as the result.\n" +
                    "Also see: bean-ls, bean-cd, bean-cat.")
    @ScriptyStdArgList
    public Object beanPwd(@ScriptyBindingParam("*output") PrintWriter writer) {
        writer.println(currentDirectory);
        return currentDirectory;
    }

    // List the contents of the denoted object.
    // The action depends on the type of the object, properties, array elements are listed.
    //
    @ScriptyCommand(name = "bean-ls", description =
            "(bean-ls <property-path> [quiet=true|false*])\n" +
                    "List the properties on the path like the contents of a folder.\n" +
                    "Also see: bean-cd, bean-cat, bean-pwd.")
    @ScriptyRefArgList(ref = "path + quiet option")
    public Object beanLs(@ScriptyParam("path") String aPath, Context aCtx, @ScriptyBindingParam("*output") PrintWriter writer, @ScriptyParam("quiet") boolean isQuiet)
    throws CommandException {
        // Resolve the path.
        final Resolution resolution = resolve(aPath, aCtx);
        final Object value = resolution.getVal();
        String typeDescr = value == null ? "null" : value.getClass().getSimpleName();
        Directory dir = list(typeDescr, value);
        if (!isQuiet && writer != null) {
            writer.println(dir.toString());
        }
        return dir;
    }

    // Convert the denoted object to a string and print it on the repl.
    // The difference between ls and cat is that ls lists the subelements while
    // cat calls the toString() method.
    //
    @ScriptyCommand(name = "bean-cat", description =
            "(bean-cat <property-path> [quiet=true|false*])\n" +
                    "Write the contents of the property specified by the path on *output, and return the property.\n" +
                    "Also see: bean-cd, bean-ls, bean-pwd.")
    @ScriptyRefArgList(ref = "path + quiet option")
    public Object beanCat(@ScriptyParam("path") String path, Context ctx, @ScriptyBindingParam("*output") PrintWriter writer, @ScriptyParam("quiet") boolean isQuiet)
    throws CommandException {
        // Resolve the path.
        final Resolution resolution = resolve(path, ctx);
        final Object value = resolution.getVal();
        if(!isQuiet) {
            writer.println(value == null ? "null" : value.toString());
        }
        return value;
    }

    // Convert a pathname to the object itself.
    // We can obtain a direct reference to the object in this way.
    // It is what the other commands do automatically. This command is in fact the
    // link between other command libraries and this one.
    //
    @ScriptyCommand(name = "bean-get", description =
            "(bean-get <path> [<base-bean>])\n" +
                    "Get the property specified by the property path. The effect is the same as a quiet bean-cat.\n" +
                    "Provide a base bean to resolve the path relative to the base and not to the current path.\n" +
                    "Also see: bean-cat, bean-set, bean-call.")
    @ScriptyStdArgList(fixed = {@ScriptyArg(name = "path", type = "String")}, optional = {@ScriptyArg(name = "bean", type = "Any")})
    public Object beanRslv(
            Context aCtx,
            @ScriptyParam("path") String aPath,
            @ScriptyParam("bean") Object bean)
    throws CommandException {

        if("".equals(bean) || bean == null) {
            // Resolve the path relative to the context.
            final Resolution resolution = resolve(aPath, aCtx);
            return resolution.getVal();
        }
        else {
            // Resolve the path relative to the bean.
            if(aPath.startsWith("/")) {
                aPath = aPath.substring(1);
            }
            List<String> pieces = canonize(aPath);
            return resolveCanonical(pieces, 0, bean);
        }
    }

    // Call java methods on an object denoted by a path.
    // The target object should be denoted by a pathname.
    // The method can be specified by a method instance or by a name (and a lookup will occur).
    // The arguments are not resolved, they are passed directly to the method. You can use the
    // rslv method above to accomplish argument resolution as well.
    //
    @ScriptyCommand(name = "bean-call", description =
            "(bean-call <path>|<bean> <method-name> <arg-1> ... <arg-n>)\n" +
                    "Call a Java method on the bean specified by the property path.\n" +
                    "See also bean-get, bean-set.")
    @ScriptyRefArgList(ref = "call")
    public Object beanCall(
            Context ctx,
            @ScriptyParam("bean") Object targetBean,
            @ScriptyParam("methodName") String methodName,
            @ScriptyParam("varargs") Object[] methodArgs)
    throws CommandException {
        try {
            if(targetBean instanceof  String) {
                final String propertyPath = (String) targetBean;
                final Resolution resolution = resolve(propertyPath, ctx);
                targetBean = resolution.getVal();
            }

            // The length of an array is not available trough reflection.
            // We simulate a method call of length().
            if("length".equals(methodName) && targetBean != null && targetBean.getClass().isArray()) {
                return Array.getLength(targetBean);
            }

            Class[] argClasses = new Class[methodArgs.length];
            for (int i = 0; i < methodArgs.length; i++) {
                argClasses[i] = (methodArgs[i] == null) ? null : methodArgs[i].getClass();
            }

            Method method = targetBean.getClass().getMethod(methodName, argClasses);
            if (method == null) {
                throw new CommandException("Method could not be resolved.");
            }

            return method.invoke(targetBean, methodArgs);
        }
        catch (Exception e) {
            throw new CommandException("Invocation failed.\n" + e.getMessage(), e);
        }
    }

    // Update the properties of a bean.
    // (bean-upd BEAN prop1=val1 prop2=val2 ...)
    //
    @ScriptyCommand(name = "bean-set", description =
            "(bean-set <path>|<bean> prop-1=val-1 ... prop-n=val-n)\n" +
                    "Set the properties of the bean specified by the bean or its path.\n" +
                    "See also: bean-get, bean-call.")
    public void beanUpd(Context ctx, Object[] args)
    throws CommandException {
        if(args.length < 3) {
            throw new CommandException("Not enough arguments in bean-upd.");
        }

        Object targetBean = args[1];

        if(targetBean instanceof  String) {
            final String propertyPath = (String) targetBean;
            final Resolution resolution = resolve(propertyPath, ctx);
            targetBean = resolution.getVal();
        }

        for (int i = 2; i < args.length; i++) {
            if (!(args[i] instanceof Pair)) {
                throw new CommandException("Pair expected in bean-upd.");
            }
            Pair lPair = (Pair) args[i];
            update(targetBean, lPair.getLeft(), lPair.getRight());
        }
    }

    private static Pattern PAT_FIELDNAME_BIS = Pattern.compile("^\\s*([^\\s\\[\\]]+)?\\s*(\\[\\s*([^\\s\\[\\]]+)\\s*\\])*\\s*$");

    /**
     * A resolution represents a path and the value found at that path.
     */
    private static class Resolution {
        private Object val;
        private String path;

        Resolution(String aPath, Object aObj) {
            path = aPath;
            val = aObj;
        }

        String getPath() {
            return path;
        }

        public Object getVal() {
            return val;
        }
    }

    private Resolution resolve(String path, Context ctx)
    throws CommandException {
        String absolutePath;
        if (path.startsWith("/")) {
            absolutePath = path;
        }
        else {
            // We have a relative path, lets try to convert it to an absolute path first.
            if ("/".equals(currentDirectory)) {
                absolutePath = "/" + path;
            }
            else {
                absolutePath = currentDirectory + "/" + path;
            }
        }

        // Remove the first slash (that represents root).
        final String relativePath = absolutePath.substring(1);
        final List<String> pathParts = canonize(relativePath);

        Object value;
        if ("".equals(relativePath)) {
            // No path to follow, we are already there: the ctx itself.
            value = ctx;
        }
        else {
            // Follow the path from root (the ctx).
            value = resolveCanonical(pathParts, 0, ctx);
        }

        final StringBuilder stringBuilder = new StringBuilder("/");
        for (int i = 0; i < pathParts.size(); i++) {
            String part = pathParts.get(i);
            stringBuilder.append(part);
            if (i < (pathParts.size() - 1)) {
                stringBuilder.append("/");
            }
        }
        return new Resolution(stringBuilder.toString(), value);
    }

    private static Object resolveCanonical(List<String> pathPieces, int currPiece, Object base)
    throws CommandException {
        if (pathPieces.size() <= currPiece) return base;
        final String lPiece = pathPieces.get(currPiece);

        // We check if the piece is a number.
        int lIdx = -1;
        try {
            lIdx = Integer.parseInt(lPiece);
        }
        catch (NumberFormatException e1) {
            // Ignore this exception!
        }

        if (base instanceof Context && lIdx < 0) {
            final Context lCtx = (Context) base;
            if (lCtx.isBound(lPiece)) {
                return resolveCanonical(pathPieces, currPiece + 1, lCtx.getBinding(lPiece));
            }
            else {
                throw new CommandException(String.format("Cannot find '%s' in context.", lPiece));
            }
        }
        else if (base instanceof Map && lIdx < 0) {
            final Map lMap = (Map) base;
            if (lMap.containsKey(lPiece))
                return resolveCanonical(pathPieces, currPiece + 1, lMap.get(lPiece));
            else
                throw new CommandException("ERROR - map.");
        }
        else if (base instanceof List) {
            final List lList = (List) base;
            if (lIdx < 0) {
                throw new CommandException("List expects an index.");
            }
            if (lIdx >= lList.size()) {
                throw new CommandException("List index out of range.");
            }
            else {
                return resolveCanonical(pathPieces, currPiece + 1, lList.get(lIdx));
            }
        }
        else if (base != null) {
            if (base.getClass().isArray()) {
                // Array rendering.
                ///////////////////
                if (lIdx >= 0 && lIdx < Array.getLength(base)) {
                    return resolveCanonical(pathPieces, currPiece + 1, Array.get(base, lIdx));
                }
                else {
                    throw new CommandException("ERROR - index out of range/wrong index type/no index." + lIdx + " " + base.getClass().getSimpleName());
                }
            }
            else {
                try {
                    // Try bean.
                    //
                    final BeanInfo lInfo = Introspector.getBeanInfo(base.getClass());
                    final PropertyDescriptor[] lProps = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps) {
                        if (lProp.getName().equals(lPiece)) {
                            Method lGetter = lProp.getReadMethod();

                            Object[] lArgs;
                            if (lIdx >= 0) lArgs = new Object[]{lIdx};
                            else lArgs = new Object[]{};

                            Object lVal = lGetter.invoke(base, lArgs);
                            return resolveCanonical(pathPieces, currPiece + 1, lVal);
                        }
                    }
                }
                catch (CommandException e) {
                    throw e;
                }
                catch (Exception e) {
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
        else {
            throw new CommandException("ERROR - null.");
        }
    }

    /**
     * Convert a relative path description containing . and .. into a path without these.
     * We calculate the effect of . and ..
     */
    private static List<String> canonize(String beanPath)
    throws CommandException {
        final String[] lPieces = beanPath.split("/");
        final List<String> lResult = new ArrayList<>(lPieces.length);
        for (String lPiece : lPieces) {
            if (".".equals(lPiece)) {
                // Skip.
            }
            else if ("..".equals(lPiece)) {
                // Delete the last part and go back to the previous part.
                int lResultSize = lResult.size();
                if (lResultSize > 0) lResult.remove(lResultSize - 1);
            }
            else {
                // Note: Could be done more efficiently using a custom parser.
                final Matcher m = PAT_FIELDNAME_BIS.matcher(lPiece);
                if (!m.matches())
                    throw new CommandException(String.format("Illegal path '%s'.", lPiece));
                // Fetch the prefix.
                final String lName = m.group(1);
                if (lName != null) lResult.add(lName);
                // Get the indices.
                int lStart = m.end(1) < 0 ? 0 : m.end(1);
                final String[] lIndices = lPiece.substring(lStart).split("\\[");
                // Skip the first empty string.
                for (int i = 1; i < lIndices.length; i++) {
                    String lIdx = lIndices[i];
                    int lClosing = lIdx.lastIndexOf(']');
                    if (lClosing > 0) lIdx = lIdx.substring(0, lClosing).trim();
                    lResult.add(lIdx);
                }
            }
        }
        return lResult;
    }

    @SuppressWarnings("unchecked")
    private void update(Object aTarget, Object aProp, Object aVal)
    throws CommandException {
        String lPropName;
        int lIdx = -1;

        if (aProp instanceof String) lPropName = (String) aProp;
        else throw new CommandException("Don't know what to do with non-string keys ... (for now).");

        try {
            // Try to interprete the property as
            // an integer.
            lIdx = Integer.parseInt(lPropName);
        }
        catch (NumberFormatException e1) {
            // Ignore this exception!
        }

        if (aTarget instanceof List) {
            final List lTargetList = (List) aTarget;
            if (lIdx < 0)
                throw new CommandException("Modifying list should use an index.");
            if (lIdx >= lTargetList.size())
                throw new CommandException("Array index out of range.");

            // TODO: catch errors on this one.
            lTargetList.set(lIdx, aVal);

        }
        else if (aTarget != null) {
            if (aTarget.getClass().isArray()) {
                // Array access.
                if (lIdx < 0)
                    throw new CommandException("Modifying array should use an index.");
                if (lIdx >= Array.getLength(aTarget))
                    throw new CommandException("Array index out of range.");

                // TODO: catch errors on this one.
                Array.set(aTarget, lIdx, aVal);
            }
            else {
                try {
                    // JavaBean access.
                    BeanInfo lInfo = Introspector.getBeanInfo(aTarget.getClass());
                    PropertyDescriptor[] lProps = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps) {
                        if (lProp.getDisplayName().equals(lPropName)) {
                            Method lMeth = lProp.getWriteMethod();
                            if (lMeth == null)
                                throw new CommandException("Property cannot be written.");

                            if (lProp.getPropertyType() != aVal.getClass() && aVal instanceof String) {
                                PropertyEditor lEditor = lProp.createPropertyEditor(aTarget);
                                lEditor.setAsText((String) aVal);
                            }
                            else lMeth.invoke(aTarget, aVal);
                        }
                    }
                }
                catch (Exception e) {
                    throw new CommandException(e.getMessage(), e);
                }
            }
        }
    }

    private Directory list(String description, Object obj)
    throws CommandException {
        Directory dir = new Directory(description);
        if (obj instanceof Context) {
            dir.setSorted(true);
            Context ctx = (Context) obj;
            Map<String, Object> dump = ctx.dumpBindings();
            for (Map.Entry<String, Object> entry : dump.entrySet()) {
                String typeDescr = entry.getValue() == null ? "null" : entry.getValue().getClass().getSimpleName();
                DirectoryEntry dirEntry = new DirectoryEntry(entry.getKey(), typeDescr, true, true);
                dir.addEntry(dirEntry);
            }
        }
        else if (obj instanceof List) {
            List lst = (List) obj;
            for (int i = 0; i < lst.size(); i++) {
                Object item = lst.get(i);
                String typeDescr = item == null ? "null" : item.getClass().getSimpleName();
                DirectoryEntry dirEntry = new DirectoryEntry(String.format("[%d]", i), typeDescr, true, true);
                dir.addEntry(dirEntry);
            }
        }
        else if (obj instanceof Map) {
            Map map = (Map) obj;

            for (Object key : map.keySet()) {
                Object item = map.get(key);
                String typeDescr = item == null ? "null" : item.getClass().getSimpleName();
                DirectoryEntry dirEntry = new DirectoryEntry(String.format("[%s]", key), typeDescr, true, true);
                dir.addEntry(dirEntry);
            }
        }
        else if (obj != null) {
            if (obj.getClass().isArray()) {
                // Array rendering.
                ///////////////////
                for (int i = 0; i < Array.getLength(obj); i++) {
                    Object item = Array.get(obj, i);
                    String typeDescr = item == null ? "null" : item.getClass().getSimpleName();
                    DirectoryEntry dirEntry = new DirectoryEntry(String.format("[%d]", i), typeDescr, true, true);
                    dir.addEntry(dirEntry);
                }
            }
            else {
                try {
                    // Try JavaBean...
                    ///////////////////
                    dir.setSorted(true);
                    BeanInfo lInfo = Introspector.getBeanInfo(obj.getClass());
                    PropertyDescriptor[] lProps = lInfo.getPropertyDescriptors();
                    for (PropertyDescriptor lProp : lProps) {
                        Method lRead = lProp.getReadMethod();
                        Method lWrite = lProp.getWriteMethod();
                        DirectoryEntry dirEntry = new DirectoryEntry(lProp.getDisplayName(), lProp.getPropertyType().getSimpleName(), lRead != null, lWrite != null);
                        dir.addEntry(dirEntry);
                    }
                }
                catch (IntrospectionException e) {
                    throw new CommandException("ERROR - ...", e);
                }
            }
        }
        else throw new CommandException("ERROR - null encountered.");
        return dir;
    }

    public static class Directory {
        private boolean isSorted = false;
        private String description;
        private List<DirectoryEntry> dir = new ArrayList<>();

        Directory(String description) {
            this.description = description;
        }

        void addEntry(DirectoryEntry entry) {
            dir.add(entry);
        }

        void setSorted(boolean sorted) {
            isSorted = sorted;
        }

        @Override
        public String toString() {
            if(isSorted) {
                dir.sort(Comparator.comparing(DirectoryEntry::getName));
            }
            StringBuilder builder = new StringBuilder();
            builder.append("Directory{").append("\n").append("type=").append(description).append("\n");
            builder.append("dir=").append("\n");
            for(DirectoryEntry entry: dir) {
                builder.append("    ").append(entry.toString()).append("\n");
            }
            builder.append('}');
            return builder.toString();
        }
    }

    public static class DirectoryEntry {
        private String name;
        private String description;
        private boolean isReadable;
        private boolean isWritable;

        DirectoryEntry(String name, String description, boolean isReadable, boolean isWritable) {
            this.name = name;
            this.description = description;
            this.isReadable = isReadable;
            this.isWritable = isWritable;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public boolean isReadable() {
            return isReadable;
        }

        public void setReadable(boolean readable) {
            isReadable = readable;
        }

        public boolean isWritable() {
            return isWritable;
        }

        public void setWritable(boolean writable) {
            isWritable = writable;
        }

        @Override
        public String toString() {
            return String.format("%-20s %-20s%8s%s", name, description, isReadable?"R":"-", isWritable?"W":"-");
        }
    }
}
