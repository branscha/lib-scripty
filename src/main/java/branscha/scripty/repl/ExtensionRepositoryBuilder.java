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
import branscha.scripty.annot.*;
import branscha.scripty.parser.CommandRepository;
import branscha.scripty.parser.Command;
import branscha.scripty.parser.MethodCommand;
import branscha.scripty.spec.args.ArgListBuilder;
import branscha.scripty.spec.args.ArgSpecException;
import branscha.scripty.spec.args.ArgList;
import branscha.scripty.spec.args.RuntimeArgList;
import branscha.scripty.spec.map.*;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

public class ExtensionRepositoryBuilder implements ExtensionManager {

    private static final String ERR010 = "ExtensionRepositoryBuilder/010: Library '%s' was marked as type INSTANCE, but it was impossible to create an instance.%nMaybe you should create an instance manually and provide the instance.";
    private static final String ERR020 = "ExtensionRepositoryBuilder/020: Class '%s' contains an unnamed standard argument list. An argument list associated with a command library must be named.";
    private static final String ERR030 = "ExtensionRepositoryBuilder/030: Class '%s' contains a standard argument list '%s' with errors.%n%s";
    private static final String ERR040 = "ExtensionRepositoryBuilder/040: Class '%s' contains an unnamed variable argument list. An argument list associated with a command library must be named.";
    private static final String ERR050 = "ExtensionRepositoryBuilder/050: Class '%s' contains a variable argument list '%s' with errors.%n%s";
    private static final String ERR060 = "ExtensionRepositoryBuilder/060: Class '%s' contains more than one argument list. Use @ScriptyNamedArgLists to associate multiple argument list specifications with a single class.";
    private static final String ERR070 = "ExtensionRepositoryBuilder/070: Class '%s' has a macro/command method '%s' is annotated with both def/set binding of the result to the context.";
    private static final String ERR080 = "ExtensionRepositoryBuilder/080: Class '%s' has a macro/command method '%s' with more than one argument list.";
    private static final String ERR090 = "ExtensionRepositoryBuilder/090: Class '%s' has a macro/command method '%s' with a reference to a named argument list '%s' which does not exist.";
    private static final String ERR100 = "ExtensionRepositoryBuilder/100: While processing a standard argument list for class '%s' on method '%s'.%n%s";
    private static final String ERR110 = "ExtensionRepositoryBuilder/110: While processing a variable argument list for class '%s' on method '%s'.%n%s";
    private static final String ERR120 = "ExtensionRepositoryBuilder/120: Method '%s' in class '%s' is annotated as command and as a macro simultaneously.";
    private static final String ERR130 = "ExtensionRepositoryBuilder/130: Method '%s' in class '%s' is not static, and there is no library instance.";
    private static final String ERR140 = "ExtensionRepositoryBuilder/140: While constructing the argument mapping for class '%s' on method '%s'.%n%s";
    private static final String ERR150 = "ExtensionRepositoryBuilder/150: Class '%s' is marked as a static library and an instance was added.";

    private CommandRepository commandRepo;
    private CommandRepository macroRepo;

    public ExtensionRepositoryBuilder() {
        commandRepo = new CommandRepository();
        macroRepo = new CommandRepository();
    }

    public void addCommand(String aName, Command command) {
        commandRepo.registerCommand(aName, command);
    }

    public void addMacro(String aName, Command macro) {
        macroRepo.registerCommand(aName, macro);
    }

    public void addLibraryClasses(Class... libraryClasses)
    throws ExtensionException {
        for (Class lClass : libraryClasses) {
            String lLibraryName = lClass.getSimpleName();
            ScriptyLibraryType lLibraryType = ScriptyLibraryType.AUTO;
            ScriptyLibrary lScriptyLibrary = (ScriptyLibrary) lClass.getAnnotation(ScriptyLibrary.class);
            if (lScriptyLibrary != null) {
                lLibraryName = lScriptyLibrary.name();
                lLibraryType = lScriptyLibrary.type();
            }


            // Try to instantiate the class in order to be able to use non static 
            // methods as well. 
            Object libInstance = null;
            if (lLibraryType != ScriptyLibraryType.STATIC) {
                try {
                    libInstance = lClass.newInstance();
                }
                catch (Exception e) {
                    // Nope we could not instantiate the library, so we 
                    // will only be able to add the static methods.
                }
            }

            if (libInstance == null && lLibraryType == ScriptyLibraryType.INSTANCE) {
                throw new ExtensionException(String.format(ERR010, lClass.getName()));
            }

            addLibrary(lLibraryName, libInstance, lClass);
        }
    }

    private void registerClassArgList(Class aClass, ScriptyStdArgList aStdLst, Map<String, RuntimeArgList> aNamedLists)
    throws ExtensionException {
        final String lName = aStdLst.name();
        if (lName.length() == 0) {
            final String lMsg = String.format(ERR020, aClass.getName());
            throw new ExtensionException(lMsg);
        }

        try {
            // Build it.
            final RuntimeArgList lTuple = ArgListBuilder.buildArgList(aStdLst);
            // Remember it for references if the argument list spec has a name that is.
            aNamedLists.put(lName, lTuple);
        }
        catch (ArgSpecException e) {
            final String lMsg = String.format(ERR030, aClass.getName(), aStdLst.name(), e.getMessage());
            throw new ExtensionException(lMsg, e);
        }
    }

    private void registerClassArgList(Class aClass, ScriptyVarArgList aVarLst, Map<String, RuntimeArgList> aNamedLists)
    throws ExtensionException {
        final String lName = aVarLst.name();
        if (lName.length() == 0) {
            final String lMsg = String.format(ERR040, aClass.getName());
            throw new ExtensionException(lMsg);
        }

        try {
            // Build it.
            final RuntimeArgList lTuple = ArgListBuilder.buildArgList(aVarLst);
            // Remember it for references if the argument list spec has a name that is.
            aNamedLists.put(lName, lTuple);
        }
        catch (ArgSpecException e) {
            final String lMsg = String.format(ERR050, aClass.getName(), aVarLst.name(), e.getMessage());
            throw new ExtensionException(lMsg, e);
        }
    }

    private void addLibrary(String libName, Object libInstance, Class clazz)
    throws ExtensionException {
        // We keep track of the named arglists in this datastructure. 
        Map<String, RuntimeArgList> lNamedArgLists = new HashMap<String, RuntimeArgList>();

        // Library (Class) annotations.
        ///////////////////////////////

        ScriptyStdArgList lStdArgListClassAnnot = (ScriptyStdArgList) clazz.getAnnotation(ScriptyStdArgList.class);
        ScriptyVarArgList lVarArgListClassAnnot = (ScriptyVarArgList) clazz.getAnnotation(ScriptyVarArgList.class);
        ScriptyNamedArgLists lNamedArgListsAnnot = (ScriptyNamedArgLists) clazz.getAnnotation(ScriptyNamedArgLists.class);

        int lClassAnnotCounter = 0;
        if (lStdArgListClassAnnot != null) lClassAnnotCounter++;
        if (lVarArgListClassAnnot != null) lClassAnnotCounter++;
        if (lNamedArgListsAnnot != null) lClassAnnotCounter++;
        if (lClassAnnotCounter > 1) {
            final String lMsg = String.format(ERR060, clazz.getName());
            throw new ExtensionException(lMsg);
        }

        if (lVarArgListClassAnnot != null) {
            registerClassArgList(clazz, lVarArgListClassAnnot, lNamedArgLists);
        }
        else if (lStdArgListClassAnnot != null) {
            registerClassArgList(clazz, lStdArgListClassAnnot, lNamedArgLists);
        }
        else if (lNamedArgListsAnnot != null) {
            ScriptyStdArgList[] lStdLists = lNamedArgListsAnnot.std();
            ScriptyVarArgList[] lVarLists = lNamedArgListsAnnot.var();

            for (ScriptyStdArgList lStd : lStdLists) {
                registerClassArgList(clazz, lStd, lNamedArgLists);
            }

            for (ScriptyVarArgList lVar : lVarLists) {
                registerClassArgList(clazz, lVar, lNamedArgLists);
            }
        }

        // Find command/macro methods.
        //////////////////////////////

        final Method[] lMethods = clazz.getMethods();

        for (Method lMethod : lMethods) {
            // Result binding
            /////////////////

            ResultMapping lResultMapping = null;
            ScriptySetBinding lSetBinding = lMethod.getAnnotation(ScriptySetBinding.class);
            ScriptyDefBinding lDefBinding = lMethod.getAnnotation(ScriptyDefBinding.class);
            if (lSetBinding != null && lDefBinding != null) {
                final String lMsg = String.format(ERR070, clazz.getName(), lMethod.getName());
                throw new ExtensionException(lMsg);
            }

            if (lSetBinding != null) {
                lResultMapping = new SetResultMapping(lSetBinding.value());
            }

            if (lDefBinding != null) {
                lResultMapping = new DefResultMapping(lDefBinding.value());
            }

            // Argument list specifications.
            ////////////////////////////////

            ArgList lArgList = null;
            Map<String, ArgMapping> lMappings = null;

            ScriptyStdArgList lStdArgListAnnot = lMethod.getAnnotation(ScriptyStdArgList.class);
            ScriptyVarArgList lVarArgListAnnot = lMethod.getAnnotation(ScriptyVarArgList.class);
            ScriptyRefArgList lRefArgListAnnot = lMethod.getAnnotation(ScriptyRefArgList.class);

            int lArgListCounter = 0;
            if (lStdArgListAnnot != null) lArgListCounter++;
            if (lVarArgListAnnot != null) lArgListCounter++;
            if (lRefArgListAnnot != null) lArgListCounter++;
            if (lArgListCounter > 1) {
                final String lMsg = String.format(ERR080, clazz.getName(), lMethod.getName());
                throw new ExtensionException(lMsg);
            }

            if (lRefArgListAnnot != null) {
                final String lRef = lRefArgListAnnot.ref();
                if (lNamedArgLists.containsKey(lRef)) {
                    RuntimeArgList lTuple = lNamedArgLists.get(lRef);
                    lArgList = lTuple.getArgList();
                    lMappings = lTuple.getArgMappings();
                }
                else {
                    final String lMsg = String.format(ERR090, clazz, lMethod.getName(), lRef);
                    throw new ExtensionException(lMsg);
                }
            }
            else if (lStdArgListAnnot != null) {
                try {
                    // Build the arglist.
                    RuntimeArgList lTuple = ArgListBuilder.buildArgList(lStdArgListAnnot);
                    // Remember it for references if the argument list spec has a name that is.
                    if (lStdArgListAnnot.name().length() > 0) {
                        lNamedArgLists.put(lStdArgListAnnot.name(), lTuple);
                    }
                    lArgList = lTuple.getArgList();
                    lMappings = lTuple.getArgMappings();
                }
                catch (ArgSpecException e) {
                    final String lMsg = String.format(ERR100, clazz.getName(), lMethod.getName(), e.getMessage());
                    throw new ExtensionException(lMsg, e);
                }
            }
            else if (lVarArgListAnnot != null) {
                try {
                    // Build the arglist.
                    RuntimeArgList lTuple = ArgListBuilder.buildArgList(lVarArgListAnnot);
                    // Remember it for references if the argument list spec has a name that is.
                    if (lVarArgListAnnot.name().length() > 0) {
                        lNamedArgLists.put(lVarArgListAnnot.name(), lTuple);
                    }
                    lArgList = lTuple.getArgList();
                    lMappings = lTuple.getArgMappings();
                }
                catch (ArgSpecException e) {
                    final String lMsg = String.format(ERR110, clazz.getName(), lMethod.getName(), e.getMessage());
                    throw new ExtensionException(lMsg, e);
                }
            }

            // Command or Macro.
            ////////////////////

            ScriptyCommand lCmdAnnot = lMethod.getAnnotation(ScriptyCommand.class);
            ScriptyMacro lMacroAnnot = lMethod.getAnnotation(ScriptyMacro.class);

            if (lCmdAnnot != null && lMacroAnnot != null) {
                // The method is annotated as a command AND macro.
                // A method can be a command or a macro, but not both things at the same time.
                throw new ExtensionException(String.format(ERR120, lMethod.getName(), clazz.getSimpleName()));
            }
            else if (!(lCmdAnnot == null && lMacroAnnot == null)) {
                int lMethodModifiers = lMethod.getModifiers();
                boolean lIsStaticMethod = Modifier.isStatic(lMethodModifiers);
                if (!lIsStaticMethod && libInstance == null) {
                    throw new ExtensionException(String.format(ERR130, lMethod.getName(), clazz.getSimpleName()));
                }

                // Construct the argument injector.
                CmdMethodInjector cmdInjector;
                try {
                    cmdInjector = CmdMethodInjectorBuilder.buildCmdMethodInjector(lMethod, lMappings);
                }
                catch (ArgMappingException e) {
                    final String lMsg = String.format(ERR140, clazz.getName(), lMethod.getName(), e.getMessage());
                    throw new ExtensionException(lMsg, e);
                }

                if (lCmdAnnot != null) {
                    // If the annotation does not contain a name, we will use the
                    // method name as a name.
                    String lCmdName = lCmdAnnot.name();
                    String cmdDesc = lCmdAnnot.description();
                    boolean isHiddenCmd = lCmdAnnot.isHidden();

                    if (lCmdName.length() == 0) {
                        lCmdName = lMethod.getName();
                    }
                    commandRepo.registerCommand(lCmdName,
                            new MethodCommand(libName, libInstance, lMethod, lArgList, cmdInjector, lResultMapping, cmdDesc, isHiddenCmd));
                }
                else {
                    String lMacroName = lMacroAnnot.name();
                    String macroDesc = lMacroAnnot.description();
                    boolean isHiddenMacro = lMacroAnnot.isHidden();

                    if (lMacroName.length() == 0) {
                        lMacroName = lMethod.getName();
                    }
                    macroRepo.registerCommand(lMacroName,
                            new MethodCommand(libName, libInstance, lMethod, lArgList, cmdInjector, lResultMapping, macroDesc, isHiddenMacro));
                }
            }
        }
    }

    public void addLibraryInstances(Object... libraryInstances)
    throws ExtensionException {
        for (Object lLib : libraryInstances) {
            String lLibraryName = lLib.getClass().getSimpleName();
            ScriptyLibraryType lLibraryType = ScriptyLibraryType.AUTO;

            ScriptyLibrary lScriptyLibrary = lLib.getClass().getAnnotation(ScriptyLibrary.class);
            if (lScriptyLibrary != null) {
                lLibraryName = lScriptyLibrary.name();
                lLibraryType = lScriptyLibrary.type();
            }

            if (lLibraryType == ScriptyLibraryType.STATIC)
                throw new ExtensionException(String.format(ERR150, lLib.getClass().getName()));

            addLibrary(lLibraryName, lLib, lLib.getClass());
        }
    }

    public CommandRepository getCommandRepository() {
        return commandRepo;
    }

    public CommandRepository getMacroRepository() {
        return macroRepo;
    }

    public void setCommandRepository(CommandRepository aCommandRepo) {
        commandRepo = aCommandRepo;
    }

    public void setMacroRepository(CommandRepository aMacroRepo) {
        macroRepo = aMacroRepo;
    }
}
