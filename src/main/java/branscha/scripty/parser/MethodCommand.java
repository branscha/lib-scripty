/*******************************************************************************
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
import branscha.scripty.spec.args.IArgList;
import branscha.scripty.spec.map.ArgListMapping;
import branscha.scripty.spec.map.ArgMappingException;
import branscha.scripty.spec.map.IResultMapping;
import branscha.scripty.spec.map.ResultMappingException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class MethodCommand
implements ICommand
{
    private IArgList argList;
    private Object instance;
    private ArgListMapping argMapping;
    private IResultMapping resultMapping;
    private Method method;

    public MethodCommand(Method aMethod)
    {
        argList = null;
        method = aMethod;
        instance = null;
        argMapping = null;
        resultMapping = null;
    }

    public MethodCommand(Object aInstance, Method aMethod, IArgList aArgList, ArgListMapping aArgListMapping, IResultMapping aResultMapping)
    {
        argList = aArgList;
        method = aMethod;
        instance = aInstance;
        argMapping = aArgListMapping;
        resultMapping = aResultMapping;
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        try
        {
            Object[] lArgs = aArgs;

            if (argList != null)
            {
                lArgs = argList.guard(aArgs, aCtx);
            }
            
            if(argMapping != null)
            {
                lArgs = argMapping.map(aEval, aCtx, lArgs);
            }
            
            Object lResult = method.invoke(instance, lArgs);

            if(resultMapping != null)
            {
                resultMapping.map(lResult, aCtx);
            }

            return lResult;
        }
        catch (IllegalAccessException e)
        {
            throw new CommandException(String.format("Command '%s' illegal access exception.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
        catch (InvocationTargetException e)
        {
            // The internal invocation threw an exception, we have to fetch the original exception.
            // We don't add our own message at this level, this handler should be transparent because it does
            // not add extra information.
            Throwable lOrigExc = e.getTargetException();
            throw new CommandException(CmdUtil.concatExceptionMessages(lOrigExc));
        }
        catch (ArgMappingException e)
        {
            throw new CommandException(String.format("Command '%s' parameter mapping exception.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
        catch (ArgSpecException e)
        {
            final String lMsg = String.format("Command '%s' was invoked with incorrect arguments.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e));
            throw new CommandException(lMsg);
        }
        catch(IllegalArgumentException e)
        {
            final Class lClass = method.getDeclaringClass();
            final String lMsg = String.format("Command '%s' internal error. One of the parameters in method '%s' in class '%s' has a wrong type.", aArgs[0], method.getName(), lClass.getName());
            throw new CommandException(lMsg);
        }
        catch (ResultMappingException e)
        {
            throw new CommandException(String.format("Command '%s' the result could not be bound to the context.\n%s", aArgs[0], CmdUtil.concatExceptionMessages(e)));
        }
    }
}
