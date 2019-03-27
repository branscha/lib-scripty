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
package branscha.scripty.testlib;

import branscha.scripty.annot.*;
import branscha.scripty.annot.*;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

@ScriptyStdArgList(
        name = "unoduo",
        fixed={
                @ScriptyArg(name="uno", type="Integer"),
                @ScriptyArg(name="duo", type="Integer")},
        named={
                @ScriptyArg(name="factor", type="Integer", optional=true, value="1")})
public class MyCommandLib
{
    @ScriptyCommand
    public static void hello(@ScriptyBindingParam("*output") PrintWriter aWriter, Object ... aArgs)
    {
        aWriter.print("Hello: ");
        for (Object lArg : aArgs) aWriter.print(lArg.toString());
        aWriter.println();
    }

    @ScriptyCommand
    @ScriptyVarArgList(vararg=@ScriptyArg(name="intlist", type="Integer"), minLength = 1)
    public static int add(
            @ScriptyParam("intlist") Object[] aIntList,
            @ScriptyBindingParam("*output")PrintWriter aWriter)
    {
        int lSum = (Integer) aIntList[0];
        for(int i = 1; i < aIntList.length; i++) lSum +=  (Integer) aIntList[i];
        aWriter.println(lSum);
        return lSum;

    }

    @ScriptyCommand
    @ScriptyStdArgList(fixed={@ScriptyArg(name="abc", type="ListOf(Integer) minLength=2")})
    public static void add2(
            @ScriptyParam("abc") List<Integer> aIntList,
            @ScriptyBindingParam("*output")PrintWriter aWriter)
    {
        int lSum = 0;
        for (int i : aIntList) lSum += i;
        aWriter.println(lSum);
    }

    @ScriptyCommand
    @ScriptyRefArgList(ref="unoduo")
    public static void sub( @ScriptyParam("uno") int first,
                            @ScriptyParam("duo") int second,
                            @ScriptyBindingParam("*output")PrintWriter aWriter)
    {
        aWriter.println(first - second );
    }
    
    @ScriptyMacro(name = "inverse")
    public static List omgekeerd(Object[] aArgs)
    {
        List aExpr = new LinkedList(Arrays.asList(aArgs));

        aExpr.remove(0);
        Collections.reverse(aExpr);
        return aExpr;
    }
    
    @ScriptyCommand(name="def-bruno")
    @ScriptyDefBinding("bruno")
    public static Object defBruno(Object[] aArgs)
    {
        return aArgs[1];
    }

    @ScriptyCommand(name="set-bruno")
    @ScriptySetBinding("bruno")
    public static Object setBruno(Object[] aArgs)
    {
        return aArgs[1];
    }

//    @ScriptyMacro(name = "xxx")
//    public static Integer omgekeerd(Integer aExpr)
//    {
//
//        return aExpr;
//    }
}
