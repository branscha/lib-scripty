package branscha.scripty.cmdlib;

import branscha.scripty.annot.*;
import branscha.scripty.parser.*;
import branscha.scripty.spec.args.ArgList;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

public class HelpLibrary {

    private static final Map<String, String> formRepo = new TreeMap<>();

    static {
        formRepo.put("quote", "Prevent automatic evaluation of an argument and use the argument as a data structure. Format: (quote <expr>)");
        formRepo.put("if", "Conditional form. Format: (if <bool-expr> <then-expr> [<else-expr>]).");
        formRepo.put("while", "Loop form. Format:  (while <bool-expr> [<expr>]).");
        formRepo.put("and", "Boolean and form, it can have many arguments. It uses short-circuit evaluation. Format (and <bool-expr>+).");
        formRepo.put("or", "Boolean or form, it can have many arguments. It uses short-circuit evaluation. Format (or <bool-expr>+).");
        formRepo.put("not", "Boolean not form, it can have a single argument. Format: (not <bool-expr>).");
        formRepo.put("set", "Set a context variable, it must be defined with defvar. Format (set name value) or (set name=value).");
        formRepo.put("let", "Parallel assignment, it creates a temporary context. Format (let ((name val) | name=val ...) <expr>).");
        formRepo.put("let*", "Sequential assignment, it creates a temporary context. Format (let* ((name val) | name=val ...) <expr>).");
        formRepo.put("get", "Get the value of a context variable. Format (get name)  or $name");
        formRepo.put("lambda", "Function literal. Format (lambda (<params>) <expr>).");
        formRepo.put("defun", "Define a function. It will be stored in the context. Format: (defun name (<params>) <expr>).");
        formRepo.put("timer", "Time the evaluation of an expression. Format (timer <expr>)");
        formRepo.put("eval", "Evaluate an expression. Format (eval <expr>)");
        formRepo.put("eq", "Compare two values. The result is a boolean value. Format (eq <expr> <expr>).");
        formRepo.put("bound?", "Verify if a binding exits with the specified name. Format (bound? name).");
        formRepo.put("progn", "Expression sequence. Format (progn <expr-1> ... <expr-n>)");
        formRepo.put("funcall", "Explicit function call.  Format (funcall name <args>) or (name <args>).");
        formRepo.put("defvar", "Define a context variable. Defining a variable will overwrite a previous definition. Format (defvar name value) or (defvar name=value).");
    }

    @ScriptyCommand(name = "help", description = "Show help information about forms (built-ins), available commands and macro's.")
    @ScriptyStdArgList(optional = {@ScriptyArg(name = "search", type = "String", optional = true)})
    public void help(
            Context ctx,
            Eval eval,
            @ScriptyBindingParam(value = "*output", unboundException = true) Writer writer,
            @ScriptyParam("search") String search)
    throws CommandException, IOException {

        CommandRepository cmdRepo = eval.getCommandRepo();
        Map<String, Command> cmdDump = cmdRepo.dumpCommands();

        CommandRepository macroRepo = eval.getMacroRepo();
        Map<String, Command> macroDump = macroRepo.dumpCommands();

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
                        .append("Commands: ")
                        .append("\n")
                        .append(cmdDump.keySet().stream().sorted().collect(Collectors.joining(", ")))
                        .append("\n")
                        .append("\n");
            }

            if (macroDump.size() > 0) {
                builder
                        .append("Macro's: ")
                        .append("\n")
                        .append(macroDump.keySet().stream().sorted().collect(Collectors.joining(", ")))
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

            found = helpInfo(writer, search, cmdDump, found);

            found = helpInfo(writer, search, macroDump, found);

            if (!found) {
                writer.write(String.format("No help about '%s'.%n", search));
            }

            writer.flush();
        }
    }

    private boolean helpInfo(Writer writer, String search, Map<String, Command> cmdDump, boolean found)
    throws IOException {
        if (cmdDump.containsKey(search)) {
            Command cmd = cmdDump.get(search);
            if (cmd instanceof MethodCommand) {
                final MethodCommand methodCommand = (MethodCommand) cmd;
                final StringBuilder builder = new StringBuilder();
                String description = methodCommand.getDescription();
                if(description == null || description.length() <= 0) {
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
