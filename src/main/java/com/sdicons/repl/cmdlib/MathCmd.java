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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class MathCmd
extends AbstractCommand
{
    public static enum CalcCmdOp {abs, add, sub, div, mult, pow, rem, fin, toInt, isZero, isGt, isGe, isLt, isLe, isEq, isNumber}

    public static class MathContextHolder
    {
        MathContext mathCtx = MathContext.DECIMAL64;

        public MathContext getMathCtx()
        {
            return mathCtx;
        }

        public void setMathCtx(MathContext mathCtx)
        {
            this.mathCtx = mathCtx;
        }
    }

    public static void registerCommands(IRegistry aReg)
    {
        MathContextHolder lMathCtx = new MathContextHolder();
        aReg.registerCommand("abs", new MathCmd(CalcCmdOp.abs, lMathCtx));
        aReg.registerCommand("+", new MathCmd(CalcCmdOp.add, lMathCtx));
        aReg.registerCommand("-", new MathCmd(CalcCmdOp.sub, lMathCtx));
        aReg.registerCommand("*", new MathCmd(CalcCmdOp.mult, lMathCtx));
        aReg.registerCommand("/", new MathCmd(CalcCmdOp.div, lMathCtx));
        aReg.registerCommand("^", new MathCmd(CalcCmdOp.pow, lMathCtx));
        aReg.registerCommand("fin", new MathCmd(CalcCmdOp.fin, lMathCtx));
        aReg.registerCommand("float->int", new MathCmd(CalcCmdOp.toInt, lMathCtx));
        aReg.registerCommand("zero?", new MathCmd(CalcCmdOp.isZero, lMathCtx));
        aReg.registerCommand(">", new MathCmd(CalcCmdOp.isGt, lMathCtx));
        aReg.registerCommand(">~", new MathCmd(CalcCmdOp.isGe, lMathCtx));
        aReg.registerCommand("<", new MathCmd(CalcCmdOp.isLt, lMathCtx));
        aReg.registerCommand("<~", new MathCmd(CalcCmdOp.isLe, lMathCtx));
        aReg.registerCommand("~", new MathCmd(CalcCmdOp.isEq, lMathCtx));
        aReg.registerCommand("number?", new MathCmd(CalcCmdOp.isNumber, lMathCtx));
        aReg.registerCommand("rem", new MathCmd(CalcCmdOp.rem, lMathCtx));
    }

    private CalcCmdOp op;
    private MathContextHolder mathCtxHldr;

    public MathCmd(CalcCmdOp aOp, MathContextHolder aCtxHolder)
    {
        op = aOp;
        mathCtxHldr = aCtxHolder;
    }

    private List<BigDecimal> guardAtLeastOneNumber(Object[] aArgs)
    throws CommandException
    {
        List<BigDecimal> lNrs = convertArguments(aArgs);
        if(lNrs.size() < 1)
            throw new CommandException(String.format("ERROR: The '%s' command expects at least one number.", aArgs[0]));
        return lNrs;
    }

    private BigDecimal guardSingleNumber(Object[] aArgs)
    throws CommandException
    {
        List<BigDecimal> lNrs = convertArguments(aArgs);
        if(lNrs.size() != 1)
            throw new CommandException(String.format("ERROR: The '%s' command expects 1 number and received %d.", aArgs[0], lNrs.size()));
        return lNrs.get(0);
    }

    private List<BigDecimal> guardExactlyTwoNumbers(Object[] aArgs)
    throws CommandException
    {
        List<BigDecimal> lNrs = convertArguments(aArgs);
        if(lNrs.size() != 2)
            throw new CommandException(String.format("ERROR: The '%s' command expects exactly 2 numbers and received %d.", aArgs[0], lNrs.size()));
        return lNrs;
    }

    public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
    throws CommandException
    {
        switch(op)
        {
            case add:
            {
                List<BigDecimal> lNrs = convertArguments(aArgs);
                BigDecimal lSum = BigDecimal.ZERO;
                for(BigDecimal lDec : lNrs)
                {
                    lSum = lSum.add(lDec, mathCtxHldr.getMathCtx());
                }
                return lSum;
            }
            case sub:
            {
                List<BigDecimal> lNrs = guardAtLeastOneNumber(aArgs);
                BigDecimal lSum = lNrs.get(0);
                for(int i = 1; i < lNrs.size(); i++)
                {
                    lSum = lSum.subtract(lNrs.get(i), mathCtxHldr.getMathCtx());
                }
                return lSum;
            }
            case div:
            {
                List<BigDecimal> lNrs = guardAtLeastOneNumber(aArgs);
                BigDecimal lResult = lNrs.get(0);
                for(int i = 1; i < lNrs.size(); i++)
                {
                    lResult = lResult.divide(lNrs.get(i), mathCtxHldr.getMathCtx());
                }
                return lResult;

            }
            case mult:
            {
                List<BigDecimal> lNrs = guardAtLeastOneNumber(aArgs);
                BigDecimal lResult = lNrs.get(0);
                for(int i = 1; i < lNrs.size(); i++)
                {
                    lResult = lResult.multiply(lNrs.get(i), mathCtxHldr.getMathCtx());
                }
                return lResult;
            }
            case pow:
            {
                try
                {
                    List<BigDecimal> lNrs = guardExactlyTwoNumbers(aArgs);
                    BigDecimal lNr = lNrs.get(0);
                    BigDecimal lPow = lNrs.get(1);
                    return lNr.pow(lPow.intValueExact(), mathCtxHldr.getMathCtx());
                }
                catch (ArithmeticException e)
                {
                    throw new CommandException(String.format("ERROR: The '%s' command expects an integer as second argument.", aArgs[0]));
                }
            }
            case rem:
            {
                List<BigDecimal> lNrs = guardExactlyTwoNumbers(aArgs);
                BigDecimal lNr = lNrs.get(0);
                BigDecimal lRem = lNrs.get(1);
                return lNr.remainder(lRem, mathCtxHldr.getMathCtx());
            }
            case isNumber:
            {
                try
                {
                    guardSingleNumber(aArgs);
                }
                catch (CommandException e)
                {
                    return false;
                }
                return true;
            }
            case abs:
            {
                return guardSingleNumber(aArgs).abs();
            }
            case fin:
            {
                return guardSingleNumber(aArgs).setScale(2, BigDecimal.ROUND_HALF_UP);
            }
            case toInt:
            {
                return guardSingleNumber(aArgs).toBigInteger();
            }
            case isZero:
            {
                return BigDecimal.ZERO.equals(guardSingleNumber(aArgs));
            }
            case isGt:
            case isGe:
            case isLt:
            case isLe:
            case isEq:
            {
                List<BigDecimal> lNrs = guardExactlyTwoNumbers(aArgs);
                BigDecimal lNr1 = lNrs.get(0);
                BigDecimal lNr2 = lNrs.get(1);
                if(op == CalcCmdOp.isGt) return lNr1.compareTo(lNr2) > 0;
                else if(op == CalcCmdOp.isGe) return lNr1.compareTo(lNr2) >= 0;
                else if (op == CalcCmdOp.isLt) return lNr1.compareTo(lNr2) < 0;
                else if (op == CalcCmdOp.isLe) return lNr1.compareTo(lNr2) <= 0;
                else return lNr1.compareTo(lNr2) == 0;
            }
        }
        return null;
    }

    private List<BigDecimal> convertArguments(Object[] aArgs)
    throws CommandException
    {
        // Initialize a fresh empty list. We will accumulate the result in here.
        List<BigDecimal> lResult = new ArrayList<BigDecimal>(aArgs.length - 1);
        // Skip the command name!
        for (int i = 1; i < aArgs.length; i++)
        {
            try
            {
                final Object lNrCandidate = aArgs[i];
                if(lNrCandidate instanceof BigDecimal)
                    // We are lucky, we can use the argument as-is.
                    lResult.add((BigDecimal) lNrCandidate);
                else if(lNrCandidate instanceof BigInteger)
                    // Almost good.
                    lResult.add(new BigDecimal((BigInteger) lNrCandidate));
                else
                {
                    // Last resort, convert to a string and try to parse.
                    final BigDecimal lDec = new BigDecimal(aArgs[i].toString());
                    lResult.add(lDec);
                }
            }
            catch (Exception e)
            {
                throw new CommandException(String.format("ERROR: Number formatting error: '%s'.", aArgs[i]));
            }
        }
        return lResult;
    }
}
