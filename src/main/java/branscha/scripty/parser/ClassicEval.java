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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * The evaluator understands a simplified lisp syntax.
 * Explicitly lacking: data structures, data structure manipulation. It should be done using Java commands and an
 * underlying Java model. The language on top should help to manipulate the underlying Java model.
 * <p>
 * Built-in commands:
 * <ul>
 * <li><b><code>quote</code></b> prevents evaluation of an expression: <code>(quote &lt;expr&gt;)</code> or the shorthand <code>'&lt;expr&gt.</code>
 * It is necessary to provide this construct so that the user can use unevaluated expressions to describe data structures or other parameters
 * that can be provided to the commands.</li>
 * <li><b><code>if</code></b> expression has the form <code>(if &lt;bool-expr> &lt;then-expr> &lt;else-expr>)</code>.
 * It is a special form because the evaluation of the <code>then-expr</code> or <code>else-expr</code> depends on the outcome of the test.
 * Since the evaluation order of all boolean constructs is deviant from normal evaluation they have to be built into the core.
 * <ul>
 * <li><i>TRUTHY</i>: yes, y, on, true, t, non-null</li>
 * <li><i>FALSY</i>: 0, no, off, false, null, empty collection, non-empty strings different from the negative values listed before.</li>
 * </ul>
 * </li>
 * <li><b><code>while</code></b> <code>(while &lt;bool-expr> &lt;expr>)</code>.
 * <li><b><code>set</code></b> changes an existing binding. It evaluates the value before setting, the result is the value: <code>(set name val) | (set name=val)</code>.
 * Set generates an error if the binding does not exist. A binding can initially be created using one of the following constructs. Afther a binding is created it
 * can be modified with <code>set</code>.
 * <ul>
 * <li>A <code>defvar</code> which creates/overwrites a global binding.
 * <li>A <code>defun</code> which (re-)binds a global variable to a lambda.
 * <li>A <code>let</code> or <code>let*</code> block which adds a number of bindings for the duration of a block.
 * <li>Some commands add something to the context too. It is up to the various commands to specify this.
 * </ul>
 * </li>
 * <li><b><code>get</code></b> retrieves a binding. It does not do any evaluation: <code>(get name)</code> or the shorthand notational convenience <b><code>$name</code></b> does exactly the same thing.
 * It does not do Perl-like interpolation in strings, don't let the notation mislead you.</li>
 * <li><b><code>defvar</code></b> creates a global binding. It evaluates the value before setting, the result is the value: <code>(defvar name val) | (defvar name=val)</code>. The value can be changed with <code>set</code>.</li>
 * <li><code><b>let, let*</b></code> defines variables locally: <code>(let ((var val) | var=val | var ...) expr)</code></li>
 * <li><code><b>bound?</b></code> See if a binding exists.</li>
 * <li><b><code>and, or, not</code></b>. Shortcut boolean evaluation operators.</li>
 * <li><b><code>eq</code></b> has Java equals semantics. It is the only data inspection that is implemented in the ClassicEval.
 * It is included because it is a standard Java Object method, applicable to all instances.</li>
 * <li><b><code>eval</code></b> evaluate an expression.</li>
 * <li><b><code>lambda</code></b> A nameless function, it contains a reference to the lexical context where it was defined. So this creates closures.</li>
 * <li><b><code>defun</code></b> <code>(defun name (&lt;params>) &lt;expr>)</code> User defined functions, they are bound in the
 * same context as the variables are. Functions are bound in the global context.
 * <ul>
 * <li><b><code>name</code></b> should be a string. The name is not evaluated.</li>
 * <li><b><code>(&lt;params>)</code></b>The parameter list a list of strings. The list is not evaluated.</li>
 * </ul>
 * </li>
 * <li><b><code>funcall</code></b> <code>(funcall name &lt;args&gt;)</code>. It is the official way to call a user defined function,
 * but the shorthand is a call of the form <code>(name arg-list)</code>. This form will lead to a function call if there was no registered command with the same name.
 * <ul>
 * <li><b><code>name</code></b> should be a string and is not evaluated.</li>
 * <li><b><code>&lt;args&gt;</code></b> The arguments in a separate list.</li>
 * </ul>
 * </li>
 * <li><b><code>progn</code></b> which accepts a list of expressions which are evaluated in order: <code>(progn expr1 expr2 ...)</code>.</li>
 * </ul>
 * <p>
 * Remarks:
 * <ul>
 * <li>Lists are Java lists and not conses. So list semantics is different (and maybe less efficient).
 * There is no 'nil' concept; a list does not end with a nil, a nil is not the same as an empty list.</li>
 * <li>No separate name spaces for different constructs, there is only a single context stack.</li>
 * <li>Contexts have side effects, bindings can be changed.</li>
 * <li>Only strings are provided, there is no 'symbol' concept. If an application wants e.g. numbers
 * for calculation, the commands should parse the strings.</li>
 * <li>Binding context lookup order. Scoping is lexical. The global context is always available for everybody,
 * there is no need to introduce dynamic scoping.
 * <ol>
 * <li>Call context, arguments are bound to parameters. This context is especially created for this call. It contains all local bindings.</li>
 * <li>Lexical (static) context, where the function or lambda was defined. It is the closure of the lambda.</li>
 * </ol></li>
 * <li>Interrupting the eval thread will lead to a CommandException. This is the preferred way to stop a runaway script.</li>
 * </ul>
 */
public class ClassicEval extends AbstractEval {

    private static final String ERR010 =
            "ClassicEval/010: Stack overflow, too many nested function calls.";
    private static final String ERR020 =
            "ClassicEval/020: Evaluation has been interrupted.";
    private static final String ERR030 =
            "ClassicEval/030: Form 'quote' should have format (quote <expr>).";
    private static final String ERR040 =
            "ClassicEval/040: Form 'if' should have format (if <bool-expr> <then-expr> [<else-expr>]).";
    private static final String ERR050 =
            "ClassicEval/050: Form 'while' should have format (while <bool-expr> [<expr>]).";
    private static final String ERR060 =
            "ClassicEval/060: Form 'and' should have format (and <bool-expr>+).";
    private static final String ERR070 =
            "ClassicEval/070: Form 'or' should have format (or <bool-expr>+).";
    private static final String ERR080 =
            "ClassicEval/080: Form 'not' should have format (not <bool-expr>).";
    private static final String ERR090 =
            "ClassicEval/090: Form '%s' should have format (%s name value).";
    private static final String ERR100 =
            "ClassicEval/100: Form '%s' should have format (%s name value).";
    private static final String ERR110 =
            "ClassicEval/110: First argument in form '%s' should evaluate to a string.";
    private static final String ERR120 =
            "ClassicEval/120: Form '%s' should have format (%s ((name val)...) <expr>).";
    private static final String ERR130 =
            "ClassicEval/130: Form '%s' should have format (%s ((name val)...) <expr>).%nFirst parameter should be a list " +
                    "of bindings and encountered an instance of type '%s'.";
    private static final String ERR140 =
            "ClassicEval/140: Form '%s' should have format (%s ((name val) | name=val ...) <expr>).%nEach binding should " +
                    "be a list of length 2 of format (var val).";
    private static final String ERR150 =
            "ClassicEval/150: Special '%s' should have format (%s ((name val) | name=val ...) <expr>).%nEach binding " +
                    "should be a list  of format (var val).%nFirst element should be a string but encountered an " +
                    "instance of type '%s'.";
    private static final String ERR160 =
            "ClassicEval/160: Form '%s' should have format (%s ((name val) | name=val ...) <expr>).%nEach binding should " +
                    "be a list or a string or a pair but encountered an instance of type '%s'.";
    private static final String ERR170 =
            "ClassicEval/170: Form 'get' should have format (get name).";
    private static final String ERR180 =
            "ClassicEval/180: First argument in form 'get' should evaluate to a string.";
    private static final String ERR190 =
            "ClassicEval/190: Form 'lambda' should have format (lambda (<params>) <expr>).";
    private static final String ERR200 =
            "ClassicEval/200: First argument in form 'lambda' should evaluate to a list of parameters.";
    private static final String ERR210 =
            "ClassicEval/210: First argument in form 'lambda', the parameter list, should evaluate to a list of strings.";
    private static final String ERR220 =
            "ClassicEval/220: Second argument in form 'lambda' should be an expression.";
    private static final String ERR230 =
            "ClassicEval/230: Form 'defun' should have format (defun name (<params>) <expr>).";
    private static final String ERR240 =
            "ClassicEval/240: First argument in form 'defun' should evaluate to a string.";
    private static final String ERR250 =
            "ClassicEval/250: Second argument in form 'defun' should evaluate to a list of parameters.";
    private static final String ERR260 =
            "ClassicEval/260: Second argument in form 'defun', the parameter list, should evaluate to a list of strings.";
    private static final String ERR270 =
            "ClassicEval/270: Third argument in form 'defun' should be an expression.";
    private static final String ERR280 =
            "ClassicEval/280: Form 'timer' should have format (timer <expr>).";
    private static final String ERR290 =
            "ClassicEval/290: Form 'eval' should have a single argument.";
    private static final String ERR300 =
            "ClassicEval/300: Form 'eq' should have two arguments.";
    private static final String ERR310 =
            "ClassicEval/310: Form 'bound?' should have a single argument.";
    private static final String ERR320 =
            "ClassicEval/320: Form 'bound?' should have a single string argument.";
    private static final String ERR330 =
            "ClassicEval/330: Form 'progn' should have at least one argument.";
    private static final String ERR340 =
            "ClassicEval/340: Form 'funcall' should have the format (funcall name <args>).";
    private static final String ERR350 =
            "ClassicEval/350: Function \"%s\" was not found in the context.";
    private static final String ERR355 =
            "ClassicEval/355: First argument in form 'funcall' should evaluate to a string " +
            "or a lambda, and we received 'null'.";
    private static final String ERR360 =
            "ClassicEval/360: First argument in form 'funcall' should evaluate to a string or lambda, and received " +
                    "an instance of class '%s'.";
    private static final String ERR370 =
            "ClassicEval/370: Function call '%s' failed.%n%s";
    private static final String ERR380 =
            "ClassicEval/380: Command '%s' failed.%n%s";
    private static final String ERR390 =
            "ClassicEval/390: Command or form '%s' does not exist.";
    private static final String ERR400 =
            "ClassicEval/400: Command name should evaluate to a string or a lambda. Found null.";
    private static final String ERR410 =
            "ClassicEval/410: Command name should evaluate to a string or a lambda.%nFound an instance '%s' of " +
                    "class \"%s\", which cannot be interpreted as a function.";
    private static final String ERR420 =
            "ClassicEval/420: Macro '%s' failed.%n%s";

    private CommandRepository commands;
    private CommandRepository macros;

    // Limit the total exception message length.
    private static final int TOTAL_MSG_LIMIT = 1000;
    // Limit a single exception entry within the composite exception.
    private static final int ENTRY_MSG_LIMIT = 80;

    public ClassicEval() {
        this(new BasicContext());
    }

    public ClassicEval(Context aContext) {
        super(aContext);
        commands = new CommandRepository();
        macros = new CommandRepository();
    }

    public void setCommandRepo(CommandRepository cmdRepository) {
        commands = cmdRepository;
    }

    public CommandRepository getCommandRepo() {
        return commands;
    }

    public void setMacroRepo(CommandRepository macroRepository) {
        macros = macroRepository;
    }

    public CommandRepository getMacroRepo() {
        return macros;
    }


    public Object eval(Object expr)
    throws CommandException {
        try {
            return eval(expr, getContext());
        }
        catch (StackOverflowError e) {
            // Protection against runaway user functions. This probably is a bug
            // like an uncontrolled recursion or a indefinite loop.
            throw new CommandException(ERR010);
        }
    }

    @SuppressWarnings("unchecked")
    public Object eval(Object expr, Context ctx)
    throws CommandException {
        try {
            // Stop evaluation if the current evaluation thread got a signal to
            // stop processing the expression.
            if (Thread.interrupted()) {
                throw new CommandException(ERR020);
            }

            if (!(expr instanceof List)) {
                // I. Atomic expressions
                ////////////////////////

                if (expr instanceof String && ((String) expr).startsWith("$")) {
                    // Syntactic sugar. $name is equivalent to (get name).
                    return ctx.getBinding(((String) expr).substring(1));
                }
                else if (expr instanceof Pair) {
                    // Pairs are meant to make key/value pairs easier.
                    final Pair pair = (Pair) expr;
                    final Object evaluatedLeft = eval(pair.getLeft(), ctx);
                    final Object evaluatedRight = eval(pair.getRight(), ctx);
                    return new Pair(evaluatedLeft, evaluatedRight);
                }
                else {
                    // All other values except lists evaluate to themselves! This is quite different from the
                    // original LISP. Our goal is to have a handy and flexible REPL that can handle model objects
                    // or "handles" that do not have a list representation.
                    ////////////////////////////////////////////
                    return expr;
                }
            }
            else {
                // II. List composite expressions
                /////////////////////////////////

                // Add a List view to the expression.
                final List<Object> exprList = (List<Object>) expr;
                final int exprListSize = exprList.size();

                // An empty list is never evaluated, but we must return a new empty list as a result in order to prevent
                // modification to the expression itself.
                if (exprListSize == 0) {
                    return new ArrayList();
                }
                Object cmdCandidate = exprList.get(0);

                // 1. Special forms are not evaluated as a normal form.
                //    Order of evaluation of the arguments can be controlled
                //    in this section.
                ////////////////////////////////////////////////////////////

                if ("quote".equals(cmdCandidate)) {
                    // If it is a quoted list, we don't evaluate its elements, the result is again, the list.
                    // In this way you can use lists as data structures and not as a function call.
                    // It is a special form because it influences the way the expression is (not) evaluated, it is non-standard.
                    if (exprListSize == 2) {
                        return exprList.get(1);
                    }
                    else {
                        throw new CommandException(ERR030);
                    }
                }
                else if ("if".equals(cmdCandidate)) {
                    // It is a special form because only one of the then or else part is evaluated depending on the outcome of the test.
                    // It shortcuts evaluation of the other branch.

                    // Quick test on the number of arguments.
                    if (exprListSize < 3 || exprListSize > 4) {
                        throw new CommandException(ERR040);
                    }

                    if (boolEval(eval(exprList.get(1), ctx))) {
                        return eval(exprList.get(2), ctx);
                    }
                    else if (exprListSize == 4) {
                        return eval(exprList.get(3), ctx);
                    }
                    else {
                        return null;
                    }
                }
                else if ("while".equals(cmdCandidate)) {
                    // It is a special form because the test is evaluated again and again.

                    // Quick test on the number of arguments.
                    if (exprListSize < 2 || exprListSize > 3)
                        throw new CommandException(ERR050);
                    Object lLastResult = null;
                    while (boolEval(eval(exprList.get(1), ctx)))
                        if (exprListSize == 3) lLastResult = eval(exprList.get(2), ctx);
                    return lLastResult;
                }
                else if ("and".equals(cmdCandidate)) {
                    // It is a special form because it shortcuts evaluation if it encounters a
                    // false value.

                    // Quick test on the number of arguments.
                    if (exprListSize <= 1) {
                        throw new CommandException(ERR060);
                    }

                    Iterator<Object> iter = exprList.iterator();
                    // Skip the "and"
                    iter.next();
                    // We evaluate the arguments until we encounter a false one.
                    // We don't evaluate the arguments after the false one.
                    while (iter.hasNext()) {
                        final Object lArg = iter.next();
                        if (!boolEval(eval(lArg, ctx))) return Boolean.FALSE;
                    }
                    return Boolean.TRUE;
                }
                else if ("or".equals(cmdCandidate)) {
                    // It is a special form because it shortcuts evaluation when a single true value
                    // was found

                    // Quick test on the number of arguments.
                    if (exprListSize <= 1)
                        throw new CommandException(ERR070);
                    Iterator<Object> lIter = exprList.iterator();
                    // Skip the "or"
                    lIter.next();
                    // We evaluate the arguments until we encounter a true one.
                    // We don't evaluate the arguments after the true one.
                    while (lIter.hasNext()) {
                        final Object lArg = lIter.next();
                        if (boolEval(eval(lArg, ctx))) return Boolean.TRUE;
                    }
                    return Boolean.FALSE;
                }
                else if ("not".equals(cmdCandidate)) {
                    // Quick test on the number of arguments.
                    if (exprListSize != 2)
                        throw new CommandException(ERR080);
                    if (boolEval(eval(exprList.get(1), ctx))) return Boolean.FALSE;
                    else return Boolean.TRUE;
                }
                else if ("set".equals(cmdCandidate) || "defvar".equals(cmdCandidate)) {
                    Object lName;
                    Object lValue;

                    if (exprListSize == 2) {
                        // Variant (xxx name=value)
                        //
                        Object lPairCand = exprList.get(1);
                        if (!(lPairCand instanceof Pair))
                            throw new CommandException(String.format(ERR090, cmdCandidate, cmdCandidate));
                        Pair lPair = (Pair) lPairCand;
                        lName = eval(lPair.getLeft(), ctx);
                        lValue = eval(lPair.getRight(), ctx);
                    }
                    else if (exprListSize == 3) {
                        // Variant (xxx name value)
                        //
                        lName = eval(exprList.get(1), ctx);
                        lValue = eval(exprList.get(2), ctx);
                    }
                    else {
                        throw new CommandException(String.format(ERR100, cmdCandidate, cmdCandidate));
                    }

                    // Check the type of the name.
                    if (!(lName instanceof String))
                        throw new CommandException(String.format(ERR110, cmdCandidate));
                    final String lNameRepr = (String) lName;

                    if ("set".equals(cmdCandidate)) ctx.setBinding(lNameRepr, lValue);
                    else ctx.getRootContext().defBinding(lNameRepr, lValue);

                    return lValue;
                }
                else if ("let".equals(cmdCandidate) || "let*".equals(cmdCandidate)) {
                    if (exprListSize != 3)
                        throw new CommandException(String.format(ERR120, cmdCandidate, cmdCandidate));
                    final Object lBindings = exprList.get(1);
                    final Object lExpr = exprList.get(2);
                    final boolean letrec = "let*".equals(cmdCandidate);

                    // Check the type of the list of bindings.
                    if (!(lBindings instanceof List))
                        throw new CommandException(String.format(ERR130, cmdCandidate, cmdCandidate, lBindings == null ? "null" : lBindings.getClass().getCanonicalName()));
                    final List lBindingsList = (List) lBindings;

                    // For let we will accumulate the eval results of the bindings
                    // in this list and add them to the context after all bindings have been evaluated.
                    final List<Pair> lBindingPrep = new ArrayList<>();
                    // The let or let* context.
                    // For let* we will incrementally use the new context.
                    final Context lLetCtx = new CompositeContext(new BasicContext(), ctx);

                    for (Object lBinding : lBindingsList) {
                        if (lBinding instanceof String) {
                            final String lName = (String) lBinding;
                            if (letrec) lLetCtx.defBinding(lName, null);
                            else lBindingPrep.add(new Pair(lName, null));
                        }
                        else if (lBinding instanceof List || lBinding instanceof Pair) {
                            Object lKey;
                            Object lValExpr;

                            if (lBinding instanceof List) {
                                // Variant (name value)
                                //
                                final List lBindingList = (List) lBinding;
                                if (lBindingList.size() != 2)
                                    throw new CommandException(String.format(ERR140, cmdCandidate, cmdCandidate));
                                lKey = lBindingList.get(0);
                                lValExpr = lBindingList.get(1);
                            }
                            else {
                                // Variant name=value
                                //
                                final Pair lBindingPair = (Pair) lBinding;
                                lKey = lBindingPair.getLeft();
                                lValExpr = lBindingPair.getRight();
                            }

                            if (!(lKey instanceof String))
                                throw new CommandException(String.format(ERR150, cmdCandidate, cmdCandidate, lKey == null ? "null" : lKey.getClass().getCanonicalName()));

                            final String lName = (String) lKey;
                            if (letrec) lLetCtx.defBinding(lName, eval(lValExpr, lLetCtx));
                            else lBindingPrep.add(new Pair(lName, eval(lValExpr, lLetCtx)));
                        }
                        else {
                            throw new CommandException(String.format(ERR160, cmdCandidate, cmdCandidate, lBinding == null ? "null" : lBinding.getClass().getCanonicalName()));
                        }
                    }

                    if (!letrec) {
                        // for let (not for let*) we will now add the bindings to the context.
                        for (Pair lPair : lBindingPrep) lLetCtx.defBinding((String) lPair.getLeft(), lPair.getRight());
                    }
                    // Evaluate the body in our freshly created context.
                    return eval(lExpr, lLetCtx);
                }
                else if ("get".equals(cmdCandidate)) {
                    // Quick test on the number of arguments.
                    if (exprListSize != 2) throw new CommandException(ERR170);
                    Object lName = eval(exprList.get(1), ctx);

                    // Check the type of the name.
                    if (!(lName instanceof String))
                        throw new CommandException(ERR180);
                    return ctx.getBinding((String) lName);
                }
                else if ("lambda".equals(cmdCandidate)) {
                    // It is a special form because the parameter list nor the function body
                    // should be evaluated at this point.

                    //Quick test on the number of arguments.
                    if (exprListSize != 3)
                        throw new CommandException(ERR190);
                    // Parameters are *not* evaluated ...
                    final Object params = exprList.get(1);
                    final Object body = exprList.get(2);

                    // Do some checking.
                    if (!(params instanceof List))
                        throw new CommandException(ERR200);
                    List lParamList = (List) params;
                    for (Object lParam : lParamList)
                        if (!(lParam instanceof String))
                            throw new CommandException(ERR210);
                    if (body == null)
                        throw new CommandException(ERR220);

                    // Construct the lambda.
                    final String[] lStrArgs = new String[lParamList.size()];
                    for (int i = 0; i < lParamList.size(); i++) lStrArgs[i] = (String) lParamList.get(i);

                    return new Lambda(lStrArgs, body, ctx);
                }
                else if ("defun".equals(cmdCandidate)) {
                    // It is a special form because the parameter list nor the function body
                    // should be evaluated at this point.

                    // Quick test on the number of arguments.
                    if (exprListSize != 4)
                        throw new CommandException(ERR230);
                    // Name and parameters are *not* evaluated ...
                    final Object lName = exprList.get(1);
                    final Object lParams = exprList.get(2);
                    final Object lBody = exprList.get(3);

                    // Do some checking.
                    if (!(lName instanceof String))
                        throw new CommandException(ERR240);
                    if (!(lParams instanceof List))
                        throw new CommandException(ERR250);
                    final List lParamList = (List) lParams;
                    for (Object lParam : lParamList)
                        if (!(lParam instanceof String))
                            throw new CommandException(ERR260);
                    if (lBody == null)
                        throw new CommandException(ERR270);

                    // Create a lambda macro.
                    final List lLambdaMacro = new ArrayList(3);
                    lLambdaMacro.add("lambda");
                    lLambdaMacro.add(lParams);
                    lLambdaMacro.add(lBody);

                    // Evaluate the lambda.
                    // Bind the resulting lambda to the name GLOBALLY!
                    final Lambda lLambda = (Lambda) eval(lLambdaMacro, ctx);
                    ctx.getRootContext().defBinding((String) lName, lLambda);
                    return lLambda;
                }
                else if ("timer".equals(cmdCandidate)) {
                    if (exprListSize != 2)
                        throw new CommandException(ERR280);

                    long lStart = System.currentTimeMillis();
                    eval(exprList.get(1), ctx);
                    long lStop = System.currentTimeMillis();
                    return lStop - lStart;
                }
                else if (cmdCandidate instanceof String && macros.hasCommand((String) cmdCandidate)) {
                    // Built-in macro call.
                    final Command lMacro = macros.getCommand((String) cmdCandidate);
                    final List lArgs = new ArrayList(exprList.size());
                    lArgs.addAll(exprList);

                    try {
                        // Macro expansion in progress!
                        //
                        return eval(lMacro.execute(this, ctx, lArgs.toArray()), ctx);
                    }
                    catch (CommandException e) {
                        // This type of error will be handled by our general mechanism.
                        // It does not need special handling here.
                        throw e;
                    }
                    catch (Exception e) {
                        // A non-CommandException is converted into a command exception here.
                        throw new CommandException(String.format(ERR420, cmdCandidate, concatExceptionMessages(e)));
                    }
                }

                // 2. All the other lists are evaluated in a standard way.
                //    This part is the standard evaluation.
                //    Order of evaluation cannot be controlled after this because
                //    they have been evaluated automatically.
                ///////////////////////////////////////////////////////////

                // Evaluation should be non destructible, therefore we must evaluate to a new list.
                // If we would replace the expressions by their evaluations, we would not
                // be able to re-evaluate expressions ...
                final List lEvalList = new ArrayList(exprList.size());

                // Evaluate all the elements in the list.
                // The number of elements should remain the same.
                for (Object lExpr : exprList)
                    lEvalList.add(eval(lExpr, ctx));

                // Re-fetch the command candidate, it might have changed by the evaluation
                cmdCandidate = lEvalList.get(0);

                // Now we try to execute the list as a command or a function.

                if (cmdCandidate instanceof String) {
                    // Create a String view on the command candidate.
                    final String cmdName = ((String) cmdCandidate).trim();

                    // Built-in forms.
                    // These are not like the special forms, because they list is
                    // evaluated like all other lists. They should be seen as built-in functionality.
                    /////////////////////////////////////////////////////////////////////////////////

                    if ("eval".equals(cmdName)) {
                        if (exprListSize != 2)
                            throw new CommandException(ERR290);
                        return eval(lEvalList.get(1), ctx);
                    }
                    else if ("eq".equals(cmdName)) {
                        if (exprListSize != 3) throw new CommandException(ERR300);
                        Object lArg1 = eval(lEvalList.get(1), ctx);
                        Object lArg2 = eval(lEvalList.get(2), ctx);
                        if (lArg1 == null && lArg2 == null) return Boolean.TRUE;
                        else if (lArg1 == null) return Boolean.FALSE;
                        else if (lArg2 == null) return Boolean.FALSE;
                        else return lArg1.equals(lArg2);
                    }
                    else if ("bound?".equals(cmdName)) {
                        if (exprListSize != 2)
                            throw new CommandException(ERR310);
                        Object lArg = lEvalList.get(1);
                        if (lArg instanceof String) {
                            String lName = (String) lArg;
                            return ctx.isBound(lName);
                        }
                        else
                            throw new CommandException(ERR320);
                    }
                    else if ("progn".equals(cmdCandidate)) {
                        if (exprListSize < 2)
                            throw new CommandException(ERR330);
                        return lEvalList.get(lEvalList.size() - 1);
                    }
                    else if ("funcall".equals(cmdName)) {
                        // This is the 'official' way of executing a method.
                        // The other methods are macro's that will sooner or later evaluate to this construct.
                        // This is the real deal (whereas the other constructs should be seen as syntactic sugar).

                        // Quick test on the number of arguments.
                        if (exprListSize < 2)
                            throw new CommandException(ERR340);
                        // Test the function name / lambda.
                        final Object lName = lEvalList.get(1);

                        // Fetch the function.
                        //////////////////////

                        Lambda lFun;
                        if (lName instanceof Lambda) {
                            // Easy part.
                            lFun = (Lambda) lName;
                        }
                        else if (lName instanceof String) {
                            // We have to do a lookup.
                            Object lFunCandidate = ctx.getBinding((String) lName);
                            if (!(lFunCandidate instanceof Lambda))
                                throw new CommandException(String.format(ERR350, lName));
                            lFun = (Lambda) lFunCandidate;
                        }
                        else {
                            // Trouble.
                            // We found something in the beginning of the list that does not evaluate to a lambda name or a lambda itself.
                            if (lName == null)
                                throw new CommandException(ClassicEval.ERR355);
                            throw new CommandException(String.format(ERR360, lName.getClass().getCanonicalName()));
                        }

                        try {
                            // Finally THE REAL DEAL!
                            /////////////////////////

                            // Context that binds the parameters to the arguments in addition to the lexical context.
                            // Note that we skip the 'funcall' constant and the function name/lambda when constructing
                            // the argument list.
                            final Context lFunCtx = lFun.createContext(lEvalList.subList(2, lEvalList.size()).toArray(), 0, lEvalList.size() - 2);
                            return eval(lFun.getExpr(), lFunCtx);
                        }
                        catch (CommandException e) {
                            // This type of error will be handled by our general mechanism.
                            // It does not need special handling here.
                            throw e;
                        }
                        catch (Exception e) {
                            // A non-CommandException is converted into a command exception here.
                            throw new CommandException(String.format(ERR370, lName, concatExceptionMessages(e)));
                        }
                    }
                    else if (commands.hasCommand(cmdName)) {
                        try {
                            final Command lCmd = commands.getCommand(cmdName);
                            return lCmd.execute(this, ctx, lEvalList.toArray());
                        }
                        catch (CommandException e) {
                            // This type of error will be handled by our general mechanism.
                            // It does not need special handling here.
                            throw e;
                        }
                        catch (Exception e) {
                            // A non-CommandException is converted into a command exception here.
                            throw new CommandException(String.format(ERR380, cmdName, concatExceptionMessages(e)));
                        }
                    }
                    else if (ctx.isBound(cmdName) && ctx.getBinding(cmdName) instanceof Lambda) {
                        // SYNTACTIC SUGAR.
                        // This is syntactic sugar so that know and declared user functions
                        // can be called without 'funcall' just like the built-in functions.

                        final List lMacro = funcallMacro(lEvalList);
                        return eval(lMacro, ctx);
                    }
                    else {
                        throw new CommandException(String.format(ERR390, cmdName));
                    }
                }
                else if (cmdCandidate instanceof Lambda) {
                    // SYNTACTIC SUGAR.
                    // This is syntactic sugar so that lambda's
                    // can be called without 'funcall' just like the built-in functions.

                    final List lMacro = funcallMacro(lEvalList);
                    return eval(lMacro, ctx);
                }
                else {
                    // Error, name of the command should be a string or a lambda.
                    if (cmdCandidate == null)
                        throw new CommandException(ERR400);
                    else
                        throw new CommandException(String.format(ERR410, cmdCandidate.toString(), cmdCandidate.getClass().getName()));
                }
            }
        }
        // The structure of the exception handling block is important:
        // * First we catch our own internal CommandExceptions.
        //   We know that this exception already contains an explanatory message, because we create these ourselves.
        //   We augment the message with a stack trace, each eval invocation will add information as
        //   the stack trace is unwound.
        // * The second clause catches general Exceptions. This indicates a programming error.
        //   Nevertheless, we catch this and turn it into our own CommandException.
        //   After this, it will participate in the stack-unwinding-info as well.
        catch (CommandException e) {
            String lMsg = e.getMessage();
            // If the message becomes too large, we will stop augmenting it in order
            // not to crash our little evaluator.
            if (lMsg.length() >= TOTAL_MSG_LIMIT) {
                if (!lMsg.endsWith("-> ..."))
                    lMsg = lMsg + "\n-> ...";
            }
            // We augment the exception with information so that the user can pinpoint the error.
            else lMsg = String.format("%s%n-> %s", e.getMessage(), limitMsg(Printer.print(expr, false)));
            throw new CommandException(lMsg);
        }
        catch (Exception e) {
            // A non-CommandException is converted into a command exception here.
            throw new CommandException(String.format("Evaluation failed.%n%s", concatExceptionMessages(e)));
        }
    }

    private String concatExceptionMessages(Throwable e) {
        final String lInitialMsg = e.getMessage() == null ? e.getClass().getSimpleName() : e.getMessage();
        final StringBuilder lBuilder = new StringBuilder(lInitialMsg);
        Throwable t = e;
        while (t.getCause() != null) {
            t = t.getCause();
            final String lMsg = t.getMessage() == null ? t.getClass().getSimpleName() : t.getMessage();
            lBuilder.append("\n").append(lMsg);
        }
        return lBuilder.toString();
    }

    private String limitMsg(Object aObj) {
        if (aObj == null) return "";
        String lMsg = aObj.toString();
        if (lMsg.length() > ENTRY_MSG_LIMIT) return lMsg.substring(0, ENTRY_MSG_LIMIT - 5) + " ...";
        else return lMsg;
    }

    @SuppressWarnings("unchecked")
    private List funcallMacro(List anExpr) {
        // Prepare a new expression.
        final List lMacro = new ArrayList(anExpr.size() + 1);
        lMacro.add("funcall");
        // We have to quote the arguments so that they are not evaluated anymore!
        // All the arguments have already been evaluated by the automatic evaluation process.
        for (Object lArg : anExpr) {
            final List lQuotedArg = new ArrayList(2);
            lQuotedArg.add("quote");
            lQuotedArg.add(lArg);
            lMacro.add(lQuotedArg);
        }
        return lMacro;
    }
}
