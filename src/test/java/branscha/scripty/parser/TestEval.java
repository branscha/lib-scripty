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

import branscha.scripty.ExtensionException;
import branscha.scripty.cmdlib.MathLibrary;
import branscha.scripty.repl.ExtensionRepositoryBuilder;
import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.junit.Assert.*;

public class TestEval {
    protected Parser parser;
    protected IEval eval;
    protected List<IEval> evals = Arrays.asList(new Eval(), new Eval2());

    @Before
    public void setup() {
        parser = new Parser();
        eval = new Eval();
    }

    @Test
    public void createBinding() {
        evals.stream().forEach((eval) -> {
            try {
                eval.eval(parser.parseExpression("(defvar oele=plopperdeplop)"));
                Object lResult = eval.eval(parser.parseExpression("$oele"));
                assertEquals("plopperdeplop", lResult);

                lResult = eval.eval(parser.parseExpression("(get oele)"));
                assertEquals("plopperdeplop", lResult);
            }
            catch (CommandException e) {
                fail(e.getMessage());
            }
        });
    }

    @Test
    public void changeBinding() {
        // Counterexample.
        // Try to change an unknown binding.
        // It should generate an error stating that you tried to change an unknown binding.

        evals.stream().forEach((eval) -> {
            try {
                eval.eval(parser.parseExpression("(set thiswasneverdefined=alloallo)"));
                fail("Should have thrown a CommandExeption.");
            }
            catch (CommandException e) {
                assertTrue(e.getMessage().indexOf("no binding") > 0);
            }
        });
    }

    @Test
    public void let()
    throws CommandException {
        // Standard let block.
        Object lResult = eval.eval(parser.parseExpression("(let (oele=bruno emptybinding boele=fons makkis=bart voele=teck) $oele)"));
        Assert.assertEquals("bruno", lResult);

        // Binding $c evaluates to $b which is evaluated in global context because of the let block.
        // This tests for the let semantics.
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 10) (defvar b 20) (defvar c 30) (let (a=13 b=$a c=$b) $c))"));
        Assert.assertEquals("20", lResult);

        // Binding $c evaluates to $b which is bound in local context because of the let* block.
        // This tests for the let* semantics.
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 10) (defvar b 20) (defvar c 30) (let* (a=13 b=$a c=$b) $c))"));
        Assert.assertEquals("13", lResult);

        // Change a defvar binding.
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 10) (set a 13) $a)"));
        Assert.assertEquals("13", lResult);

        // Change a let binding.
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 101) (let (a=13 (b 5)) (set a=17)) $a)"));
        Assert.assertEquals("101", lResult);

        // Change a let* binding.
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 113) (let* (a=13) (set a=17)) $a)"));
        Assert.assertEquals("113", lResult);
    }

    @Test
    public void lambda()
    throws CommandException {
        Object lResult = eval.eval(parser.parseExpression("(let (fie=(lambda (par) (if (eq $par uno) een twee )) arg=uno) ($fie $arg))"));
        Assert.assertEquals("een", lResult);

        lResult = eval.eval(parser.parseExpression("(let (fie=(lambda (par) (if (eq $par uno) een twee )) arg=duo) ($fie $arg))"));
        Assert.assertEquals("twee", lResult);
    }

    @Test
    public void booleanstuff()
    throws CommandException, ExtensionException {
        // Boolean primitives.
        // Truthy.
        Object lResult = eval.eval(parser.parseExpression("(if (and true True TRue on ON On oN Yes yes YES yEs t T) pos neg)"));
        Assert.assertEquals("pos", lResult);
        // Falsy.
        lResult = eval.eval(parser.parseExpression("(if (or false False Off OFF NO no nO f 0 F) pos neg)"));
        Assert.assertEquals("neg", lResult);

        // eq operator
        lResult = eval.eval(parser.parseExpression("(if (eq a a) equal different)"));
        Assert.assertEquals("equal", lResult);
        lResult = eval.eval(parser.parseExpression("(if (eq a b) equal different)"));
        Assert.assertEquals("different", lResult);

        // or
        lResult = eval.eval(parser.parseExpression("(if (or (eq a b) (eq c c)) pos neg)"));
        Assert.assertEquals("pos", lResult);
        lResult = eval.eval(parser.parseExpression("(if (or (eq a b) (eq c d)) pos neg)"));
        Assert.assertEquals("neg", lResult);

        // and
        lResult = eval.eval(parser.parseExpression("(if (and (eq a a) (eq c c)) pos neg)"));
        Assert.assertEquals("pos", lResult);
        lResult = eval.eval(parser.parseExpression("(if (and (eq a a) (eq c d)) pos neg)"));
        Assert.assertEquals("neg", lResult);

        // not
        lResult = eval.eval(parser.parseExpression("(if (not (and (eq a b) (eq c c))) pos neg)"));
        Assert.assertEquals("pos", lResult);
        lResult = eval.eval(parser.parseExpression("(if (not (and (eq a a) (eq c c))) pos neg)"));
        Assert.assertEquals("neg", lResult);

        // while
        ExtensionRepositoryBuilder lExtBldr = new ExtensionRepositoryBuilder();
        lExtBldr.addLibraryClasses(MathLibrary.class);
        eval.setCommandRepo(lExtBldr.getCommandRepository());
        eval.setMacroRepo(lExtBldr.getMacroRepository());

        lResult = eval.eval(parser.parseExpression("(progn (defvar a 10) (while (not (zero? $a)) (set a (- $a 1))) $a)"));
        Assert.assertEquals(BigDecimal.ZERO, lResult);

        // while without body
        lResult = eval.eval(parser.parseExpression("(progn (defvar a 10) (while (not (zero? (set a (- $a 1))))) $a)"));
        Assert.assertEquals(BigDecimal.ZERO, lResult);
    }

    @Test
    public void call()
    throws CommandException, ExtensionException {
        ExtensionRepositoryBuilder lExtBldr = new ExtensionRepositoryBuilder();
        lExtBldr.addLibraryClasses(MathLibrary.class);
        eval.setCommandRepo(lExtBldr.getCommandRepository());
        eval.setMacroRepo(lExtBldr.getMacroRepository());

        eval.eval(parser.parseExpression("(defun fac (n) (if (> $n 0) (* $n (fac (- $n 1))) 1))"));
        eval.eval(parser.parseExpression(
                "(defun fib (n) " +
                        "(if (~ $n 0) " +
                        "0 " +
                        "(if (~ $n 1 ) " +
                        "1 " +
                        "(+ (fib (- $n 1)) (fib (- $n 2) )))))"));

        // Plain call.
        Object lResult = eval.eval(parser.parseExpression("(fac 10)"));
        Assert.assertEquals(new BigDecimal("3628800"), lResult);
        // Some more work ...
        lResult = eval.eval(parser.parseExpression("(fib 12)"));
        Assert.assertEquals(new BigDecimal("144"), lResult);

        // Direct funcall.
        lResult = eval.eval(parser.parseExpression("(funcall fac 10)"));
        Assert.assertEquals(new BigDecimal("3628800"), lResult);
        // Lambda call
        lResult = eval.eval(parser.parseExpression("((lambda (x) (+ $x 1)) 13)"));
        Assert.assertEquals(new BigDecimal("14"), lResult);
        // Lambda call
        lResult = eval.eval(parser.parseExpression("(funcall (lambda (x) (+ $x 7)) 5)"));
        Assert.assertEquals(new BigDecimal("12"), lResult);

        // Timer.
        lResult = eval.eval(parser.parseExpression("(timer (fac 10))"));
        Assert.assertTrue((lResult instanceof Long));
    }

    @Test
    public void eval()
    throws CommandException, ExtensionException {
        ExtensionRepositoryBuilder lExtBldr = new ExtensionRepositoryBuilder();
        lExtBldr.addLibraryClasses(MathLibrary.class);
        eval.setCommandRepo(lExtBldr.getCommandRepository());
        eval.setMacroRepo(lExtBldr.getMacroRepository());

        // Simple expression evaluation.
        Object lResult = eval.eval(parser.parseExpression("(eval '(+ 21 7))"));
        Assert.assertEquals(new BigDecimal("28"), lResult);
    }

    @Test
    public void macro()
    throws CommandException {
        eval.getMacroRepo().registerCommand("unless", new ICommand() {

//            public Object transform(List anExpression)
//                    throws CommandException
//            {
//                Object lBoolExpr = anExpression.get(1);
//                Object lThenPart = anExpression.get(2);
//                Object lElsePart = anExpression.get(3);
//                List lMacro = new LinkedList();
//                lMacro.add("if");
//
//                List lNot = new LinkedList();
//                lNot.add("not");
//                lNot.add(lBoolExpr);
//
//                lMacro.add(lNot);
//                lMacro.add(lThenPart);
//                lMacro.add(lElsePart);
//
//                return lMacro;
//            }

            // (unless boolexpr then else)  => (if (not boolexpr) then else)
            public Object execute(IEval aEval, IContext aCtx, Object[] aArgs)
            throws CommandException {
                List anExpression = Arrays.asList(aArgs);
                Object lBoolExpr = anExpression.get(1);
                Object lThenPart = anExpression.get(2);
                Object lElsePart = anExpression.get(3);
                List lMacro = new LinkedList();
                lMacro.add("if");

                List lNot = new LinkedList();
                lNot.add("not");
                lNot.add(lBoolExpr);

                lMacro.add(lNot);
                lMacro.add(lThenPart);
                lMacro.add(lElsePart);

                return lMacro;
            }
        });

        // Simple expression evaluation.
        Object lResult = eval.eval(parser.parseExpression("(unless false bingo oops)"));
        Assert.assertEquals("bingo", lResult);

        // Simple expression evaluation.
        lResult = eval.eval(parser.parseExpression("(unless true bingo oops)"));
        Assert.assertEquals("oops", lResult);
    }
}
