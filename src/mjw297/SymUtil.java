package mjw297;

import java_cup.runtime.Symbol;

/* Lexing helper class */
public class SymUtil {
    /* get string literals for Symbols */
    public static String symToLiteral(Symbol sym) {
        String terminalName = Sym.terminalNames[sym.sym];
        switch (terminalName) {
            case ("EOF"): return "EOF";
            case ("error"): return "error";
            case ("MINUS"): return "-";
            case ("BANG"): return "!";
            case ("STAR"): return "*";
            case ("HIGHMULT"): return "*>>";
            case ("DIV"): return "/";
            case ("MOD"): return "%";
            case ("PLUS"): return "+";
            case ("EQ"): return "=";
            case ("LT"): return "<";
            case ("LTE"): return "<=";
            case ("GTE"): return ">=";
            case ("GT"): return ">";
            case ("EQEQ"): return "==";
            case ("NEQ"): return "!=";
            case ("AMP"): return "&";
            case ("BAR"): return "|";
            case ("SEMICOLON"): return ";";
            case ("LPAREN"): return "(";
            case ("RPAREN"): return ")";
            case ("LBRACKET"): return "[";
            case ("RBRACKET"): return "]";
            case ("LBRACE"): return "{";
            case ("RBRACE"): return "}";
            case ("UNDERSCORE"): return "_";
            case ("COMMA"): return ",";
            case ("COLON"): return ":";
            case ("STRING"): return "string " + (String) sym.value;
            case ("CHAR"): return "character " + (Character) sym.value;
            case ("ID"): return "id " + (String) sym.value;
            case ("NUM"): return "integer " + (Long) sym.value;
            case ("BIG_NUM"): return "integer " + (Long) sym.value;
            case ("WHILE"):
            case ("INT"):
            case ("BOOL"):
            case ("IF"):
            case ("ELSE"):
            case ("RETURN"):
            case ("USE"):
            case ("LENGTH"):
            case ("TRUE"):
            case ("FALSE"): return terminalName.toLowerCase();
            default: return terminalName.toLowerCase();
        }
    }
}
