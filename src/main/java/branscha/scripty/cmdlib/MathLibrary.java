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
import branscha.scripty.spec.type.TypeSpecException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import static branscha.scripty.spec.type.BigDecimalType.BIGDECIMAL_TYPE;

@SuppressWarnings("ALL")
@ScriptyNamedArgLists(
        std = {
                @ScriptyStdArgList(name = "2numbers", fixed = {@ScriptyArg(name = "arg1", type = "BigDecimal"), @ScriptyArg(name = "arg2", type = "BigDecimal")}),
                @ScriptyStdArgList(name = "1object", fixed = {@ScriptyArg(name = "arg", type = "Any")}),
                @ScriptyStdArgList(name = "1number", fixed = {@ScriptyArg(name = "arg", type = "BigDecimal")})
        },
        var = {
                @ScriptyVarArgList(name = "numbers*", fixed = {}, named = {}, vararg = @ScriptyArg(name = "numbers", type = "BigDecimal")),
                @ScriptyVarArgList(name = "numbers+", fixed = {}, named = {}, vararg = @ScriptyArg(name = "numbers", type = "BigDecimal"), minLength = 1)}
)
@ScriptyLibrary(type = ScriptyLibraryType.AUTO)
public class MathLibrary {

    private static final String ERR010 =
            "MathLibrary/010: Expected an integer as second argument, the power has to be an integer.";

    private MathContext mathCtx = MathContext.DECIMAL64;

    @ScriptyCommand(name = "+", description =
            "(+ <number-expr>*)\n" +
                    "Add zero or more numbers.")
    @ScriptyRefArgList(ref = "numbers*")
    public BigDecimal add(@ScriptyParam("numbers") Object[] aNumbers) {
        BigDecimal lSum = BigDecimal.ZERO;
        for (Object lDec : aNumbers) {
            lSum = lSum.add((BigDecimal) lDec, mathCtx);
        }
        return lSum;
    }

    @ScriptyCommand(name = "-", description =
            "(- <number-expr>+)\n" +
                    "Subtract one or more numbers.")
    @ScriptyRefArgList(ref = "numbers+")
    public BigDecimal sub(@ScriptyParam("numbers") Object[] aNumbers) {
        BigDecimal lSum = (BigDecimal) aNumbers[0];
        for (int i = 1; i < aNumbers.length; i++) {
            lSum = lSum.subtract((BigDecimal) aNumbers[i], mathCtx);
        }
        return lSum;
    }

    @ScriptyCommand(name = "/", description =
            "(/ <number-expr>+)\n" +
                    "Divide one or more numbers.")
    @ScriptyRefArgList(ref = "numbers+")
    public BigDecimal div(@ScriptyParam("numbers") Object[] aNumbers) {
        BigDecimal lResult = (BigDecimal) aNumbers[0];
        for (int i = 1; i < aNumbers.length; i++) {
            lResult = lResult.divide((BigDecimal) aNumbers[i], mathCtx);
        }
        return lResult;
    }

    @ScriptyCommand(name = "*", description =
            "(* <number-expr>+)\n" +
                    "Multiply one or more numbers.")
    @ScriptyRefArgList(ref = "numbers+")
    public BigDecimal mult(@ScriptyParam("numbers") Object[] aNumbers) {
        BigDecimal lResult = (BigDecimal) aNumbers[0];
        for (int i = 1; i < aNumbers.length; i++) {
            lResult = lResult.multiply((BigDecimal) aNumbers[i], mathCtx);
        }
        return lResult;
    }

    @ScriptyCommand(name = "^", description = "(^ number-1 integer-2)\n" +
            "Raise number-1 to the power of integer-2.")
    @ScriptyRefArgList(ref = "2numbers")
    public BigDecimal pow(@ScriptyParam("arg1") BigDecimal aNr, @ScriptyParam("arg2") BigDecimal aPow)
    throws CommandException {
        try {
            return aNr.pow(aPow.intValueExact(), mathCtx);
        }
        catch (ArithmeticException e) {
            throw new CommandException(ERR010);
        }
    }

    @ScriptyCommand(name = "rem", description =
            "(rem number-1 number-2)\n" +
                    "Remainder of the division of number-1 by number-2.")
    @ScriptyRefArgList(ref = "2numbers")
    public BigDecimal rem(@ScriptyParam("arg1") BigDecimal aNr, @ScriptyParam("arg2") BigDecimal aRem)
    throws CommandException {
        return aNr.remainder(aRem, mathCtx);
    }

    @ScriptyCommand(name = "number?", description =
            "(number? <expr>)\n" +
            "Verify if the expr can be converted to a number.")
    @ScriptyRefArgList(ref = "1object")
    public boolean isNumber(@ScriptyParam("arg") Object aArg, Context aCtx) {
        try {
            BIGDECIMAL_TYPE.guard(aArg, aCtx);
        }
        catch (TypeSpecException e) {
            return false;
        }
        return true;
    }

    @ScriptyCommand(name = "abs", description =
            "(abs number-1)\n" +
                    "Absolute value of number-1.")
    @ScriptyRefArgList(ref = "1number")
    public BigDecimal abs(@ScriptyParam("arg") BigDecimal aArg) {
        return aArg.abs();
    }

    @ScriptyCommand(name = "fin", description =
            "(fin number-1)\n" +
                    "Format arg as financial number with two decimal places.")
    @ScriptyRefArgList(ref = "1number")
    public BigDecimal fin(@ScriptyParam("arg") BigDecimal aArg) {
        return aArg.setScale(2, BigDecimal.ROUND_HALF_UP);
    }

    @ScriptyCommand(name = "float->int", description =
            "(float->int number-1)\n" +
                    "Convert number-1 to an integer.")
    @ScriptyRefArgList(ref = "1number")
    public BigInteger toInt(@ScriptyParam("arg") BigDecimal aArg) {
        return aArg.toBigInteger();
    }

    @ScriptyCommand(name = "zero?", description =
            "(zero? number-1)\n" +
                    "Verify if number-1 is zero.")
    @ScriptyRefArgList(ref = "1number")
    public boolean isZero(@ScriptyParam("arg") BigDecimal aArg) {
        return BigDecimal.ZERO.equals(aArg);
    }

    @ScriptyCommand(name = "<", description =
            "(< number-1 number-2)\n" +
                    "Less than.")
    @ScriptyRefArgList(ref = "2numbers")
    public boolean isLT(@ScriptyParam("arg1") BigDecimal aArg1, @ScriptyParam("arg2") BigDecimal aArg2) {
        return aArg1.compareTo(aArg2) < 0;
    }

    @ScriptyCommand(name = "<=", description =
            "(<= number-1 number-2)\n" +
                    "Less than or equal.")
    @ScriptyRefArgList(ref = "2numbers")
    public boolean isLE(@ScriptyParam("arg1") BigDecimal aArg1, @ScriptyParam("arg2") BigDecimal aArg2) {
        return aArg1.compareTo(aArg2) <= 0;
    }

    @ScriptyCommand(name = ">", description =
            "(> number-1 number-2)\n" +
                    "Greater than.")
    @ScriptyRefArgList(ref = "2numbers")
    public boolean isGT(@ScriptyParam("arg1") BigDecimal aArg1, @ScriptyParam("arg2") BigDecimal aArg2) {
        return aArg1.compareTo(aArg2) > 0;
    }

    @ScriptyCommand(name = ">=", description =
            "(>= number-1 number-2)\n" +
                    "Greater than or equal.")
    @ScriptyRefArgList(ref = "2numbers")
    public boolean isGE(@ScriptyParam("arg1") BigDecimal aArg1, @ScriptyParam("arg2") BigDecimal aArg2) {
        return aArg1.compareTo(aArg2) >= 0;
    }

    @ScriptyCommand(name = "=", description =
            "(= number-1 number-2)\n" +
                    "Numerical comparison.")
    @ScriptyRefArgList(ref = "2numbers")
    public boolean isEQ(@ScriptyParam("arg1") BigDecimal aArg1, @ScriptyParam("arg2") BigDecimal aArg2) {
        return aArg1.compareTo(aArg2) == 0;
    }
}
