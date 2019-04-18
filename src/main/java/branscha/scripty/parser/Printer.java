/*
 * Copyright 2019 NGDATA nv
 */

package branscha.scripty.parser;

import java.util.List;

/**
 * Utility class to render S-expressions to Strings. Strings with whitespace will become delimited, and
 * tabs and newlines will be encoded.
 */
public class Printer {

    private static final String INDENT = "  ";

    public static String print(Object sExpr, boolean pretty) {
        StringBuilder builder = new StringBuilder();
        Printer.print(builder, sExpr, pretty, INDENT);
        return builder.toString();
    }

    private static void print(StringBuilder builder, Object sExpr, boolean pretty, String indent) {
        if (sExpr == null) {
            builder.append("");
        }
        else {
            if (sExpr instanceof Pair) {
                printPair(builder, (Pair) sExpr, pretty, indent);
            }
            else if (sExpr instanceof List) {
                printList(builder, (List<Object>) sExpr, pretty, indent);
            }
            else if (sExpr instanceof String) {
                String str = sExpr.toString();
                if (str.matches(".*\\s+.*")) {
                    // The string contains whitespace.
                    // In order to preserve it we must wrap it with double quotes and translate the whitespace.
                    str = str.replace("\\", "\\\\");
                    str = str.replace("\"", "\\");
                    str = str.replace("\t", "\\t");
                    str = str.replace("\n", "\\n");
                    str = "\"" + str + "\"";
                }
                builder.append(str);
            }
            else {
                builder.append(sExpr);
            }
        }
    }

    private static void printList(StringBuilder builder, List<Object> sExpr, boolean pretty, String indent) {
        builder.append('(');
        for (int i = 0; i < sExpr.size(); i++) {
            Object subExpr = sExpr.get(i);
            if (pretty && i > 0) {
                builder.append(indent);
            }
            print(builder, subExpr, pretty, indent + INDENT);
            if (i < (sExpr.size() - 1)) {
                // Not the last list element.
                if (pretty) {
                    builder.append('\n');
                } else {
                    builder.append(' ');
                }
            }
        }
        builder.append(')');
    }

    private static void printPair(StringBuilder builder, Pair pair, boolean pretty, String indent) {
        builder.append(Printer.print(pair.getLeft(), false));
        builder.append("=");
        builder.append(Printer.print(pair.getRight(), false));
    }
}
