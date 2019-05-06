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
package branscha.scripty.parser;

import branscha.scripty.cmdlib.CmdUtil;
import branscha.scripty.spec.args.ArgSpecException;
import branscha.scripty.spec.args.ArgList;
import branscha.scripty.spec.map.CmdMethodInjector;
import branscha.scripty.spec.map.ArgMappingException;
import branscha.scripty.spec.map.ResultMapping;
import branscha.scripty.spec.map.ResultMappingException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Scripty extension implemented by a Java method.
 */
public class MethodCommand implements Command {

    private static final String ERR010 = "MethodCommand/010: Command '%s' illegal access exception.%n%s";
    private static final String ERR020 = "MethodCommand/020: Command '%s' parameter mapping exception.%n%s";
    private static final String ERR030 = "MethodCommand/030: Command '%s' was invoked with incorrect arguments.%n%s";
    private static final String ERR040 = "MethodCommand/040: Command '%s' internal error. One of the parameters in method '%s' in class '%s' has a wrong type.";
    private static final String ERR050 = "MethodCommand/050: Command '%s' the result could not be bound to the context.%n%s";

    private ArgList argList;
    private Object instance;
    private CmdMethodInjector cmdMethodInjector;
    private ResultMapping resultMapping;
    private Method method;
    private String description;
    private boolean hidden;
    private String libName;

    public MethodCommand(Method aMethod) {
        argList = null;
        method = aMethod;
        instance = null;
        cmdMethodInjector = null;
        resultMapping = null;
        description = "";
        libName = "";
    }

    public MethodCommand(String libName, Object instance, Method method, ArgList argList, CmdMethodInjector cmdMethodInjector,
                         ResultMapping resultMapping, String description, boolean hidden) {
        this.argList = argList;
        this.method = method;
        this.instance = instance;
        this.cmdMethodInjector = cmdMethodInjector;
        this.resultMapping = resultMapping;
        this.description = description;
        this.hidden = hidden;
        this.libName = libName;
    }

    public Object execute(Eval eval, Context ctx, Object[] args)
    throws CommandException {
        try {
            Object[] guarded = args;

            if (argList != null) {
                // Verify the arguments, to the conversions.
                guarded = argList.guard(args, ctx);
            }

            Object[] cmdArgs = guarded;
            if (cmdMethodInjector != null) {
                // Extract the method arguments.
                cmdArgs = cmdMethodInjector.map(eval, ctx, guarded);
            }

            Object result = method.invoke(instance, cmdArgs);

            if (resultMapping != null) {
                resultMapping.map(result, ctx);
            }

            return result;
        }
        catch (IllegalAccessException e) {
            throw new CommandException(String.format(ERR010, args[0], CmdUtil.concatExceptionMessages(e)));
        }
        catch (InvocationTargetException e) {
            // The internal invocation threw an exception, we have to fetch the original exception.
            // We don't add our own message at this level, this handler should be transparent because it does
            // not add extra information.
            Throwable lOrigExc = e.getTargetException();
            throw new CommandException(CmdUtil.concatExceptionMessages(lOrigExc));
        }
        catch (ArgMappingException e) {
            throw new CommandException(String.format(ERR020, args[0], CmdUtil.concatExceptionMessages(e)));
        }
        catch (ArgSpecException e) {
            final String lMsg = String.format(ERR030, args[0], CmdUtil.concatExceptionMessages(e));
            throw new CommandException(lMsg);
        }
        catch (IllegalArgumentException e) {
            final Class lClass = method.getDeclaringClass();
            final String lMsg = String.format(ERR040, args[0], method.getName(), lClass.getName());
            throw new CommandException(lMsg);
        }
        catch (ResultMappingException e) {
            throw new CommandException(String.format(ERR050, args[0], CmdUtil.concatExceptionMessages(e)));
        }
    }

    public String getDescription() {
        return description;
    }

    public ArgList getArgList() {
        return argList;
    }

    public boolean isHidden() {
        return hidden;
    }

    public String getLibName() {
        return libName;
    }
}
