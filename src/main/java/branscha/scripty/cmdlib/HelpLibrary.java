package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;
import branscha.scripty.parser.*;
import branscha.scripty.spec.args.ArgList;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

@SuppressWarnings("unused")
public class HelpLibrary {

    private static final Map<String, String> formRepo = new TreeMap<>();

    static {
        formRepo.put("quote", "(quote <expr>)\nPrevent automatic evaluation of an argument and use the argument as a data structure.");
        formRepo.put("if", "(if <bool-expr> <then-expr> [<else-expr>])\nConditional form.");
        formRepo.put("while", "(while <bool-expr> [<expr>])\nWhile loop form.");
        formRepo.put("and", "(and <bool-expr>+)\nBoolean and form, it can have many arguments and uses short-circuit evaluation.");
        formRepo.put("or", "(or <bool-expr>+)\nBoolean or form, it can have many arguments and uses short-circuit evaluation.");
        formRepo.put("not", "(not <bool-expr>)\nBoolean not form, it can have a single argument.");
        formRepo.put("set", "(set name value) or (set name=value)\nSet a context variable, it must be defined with defvar.");
        formRepo.put("let", "(let ((name val) | name=val ...) <expr>)\nParallel assignment, it creates a temporary context.");
        formRepo.put("let*", "(let* ((name val) | name=val ...) <expr>)\nSequential assignment, it creates a temporary context.");
        formRepo.put("get", "(get name)  or $name\nGet the value of a context variable.");
        formRepo.put("lambda", "(lambda (<params>) <expr>)\nAnonymous function literal.");
        formRepo.put("defun", "(defun name (<params>) <expr>)\nDefine a function. It will be stored in the context.");
        formRepo.put("timer", "(timer <expr>)\nTime the evaluation of an expression in milliseconds.");
        formRepo.put("eval", "(eval <expr>)\nEvaluate the expression.");
        formRepo.put("eq", "(eq <expr> <expr>)\nCompare two expressions. The result is a boolean value.");
        formRepo.put("bound?", "(bound? name)\nVerify if a binding exits with the specified name.");
        formRepo.put("progn", "(progn <expr-1> ... <expr-n>)\nExpression sequence.");
        formRepo.put("funcall", "(funcall name <args>) or (name <args>)\nExplicit function call.");
        formRepo.put("defvar", "(defvar name value) or (defvar name=value)\nDefine a context variable. Defining a variable will overwrite a previous definition.");
    }

    @ScriptyCommand(name = "help", description = "Show help information about forms (built-ins), available commands and macro's.")
    @ScriptyStdArgList(optional = {@ScriptyArg(name = "search", type = "String", optional = true)})
    public void help(
            Context ctx,
            Eval eval,
            @ScriptyBindingParam(value = "*output", unboundException = true) Writer writer,
            @ScriptyParam("search") String search)
    throws IOException {

        final Map<String, Command> cmdDump = eval.getCommandRepo().dumpCommands();
        final Map<String, Command> macroDump = eval.getMacroRepo().dumpCommands();

        // Combine the dumps in a single map.
        Map<String, MethodCommand> cmdAndMacros = new HashMap<>();
        copyNonHiddenMethodCommands(cmdDump, cmdAndMacros);
        copyNonHiddenMethodCommands(macroDump, cmdAndMacros);

        if (search.length() == 0) {
            // There is no search argument, we will print all the forms, commands and macro's.

            final StringBuilder builder = new StringBuilder();
            builder
                    .append("Forms: ")
                    .append("\n")
                    .append(formRepo.keySet().stream().sorted().collect(Collectors.joining(", ")))
                    .append("\n")
                    .append("\n");

            if (cmdDump.size() > 0) {
                builder
                        .append("Commands & Macro's: ")
                        .append("\n")
                        .append(cmdAndMacros.keySet().stream().sorted().collect(Collectors.joining(", ")))
                        .append("\n")
                        .append("\n");
            }
            writer.write(builder.toString());
            writer.flush();
        }
        else {
            boolean found = false;
            if (formRepo.containsKey(search)) {
                writer.write(formRepo.get(search) + "\n");
                found = true;
            }
            found = helpInfo(writer, search, cmdAndMacros, found);
            if (!found) {
                writer.write(String.format("No help about '%s'.%n", search));
            }
            writer.flush();
        }
    }

    private void copyNonHiddenMethodCommands(Map<String, Command> cmdDump, Map<String, MethodCommand> cmdAndMacros) {
        for (String cmdName : cmdDump.keySet()) {
            Command cmd = cmdDump.get(cmdName);
            if (cmd instanceof MethodCommand) {
                MethodCommand methodCmd = (MethodCommand) cmd;
                if (!methodCmd.isHidden()) {
                    cmdAndMacros.put(cmdName, methodCmd);
                }
            }
        }
    }

    private boolean helpInfo(Writer writer, String search, Map<String, MethodCommand> cmdDump, boolean found)
    throws IOException {
        if (cmdDump.containsKey(search)) {
            final MethodCommand methodCommand = cmdDump.get(search);
            if (!methodCommand.isHidden()) {
                final StringBuilder builder = new StringBuilder();
                String description = methodCommand.getDescription();
                if (description == null || description.length() <= 0) {
                    description = search;
                }
                builder
                        .append(description)
                        .append("\n");
                ArgList argList = methodCommand.getArgList();
                if (argList != null) {
                    builder.append(argList.toString()).append("\n");
                }
                writer.write(builder.toString());
                found = true;
            }
        }
        return found;
    }
}